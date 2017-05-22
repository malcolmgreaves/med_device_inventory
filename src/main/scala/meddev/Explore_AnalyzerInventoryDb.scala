package meddev

import java.io._
import java.math.BigInteger

import cmd.RunnerHelpers.time
import rapture.json._
import rapture.json.jsonBackends.jawn._

import scala.io.Source

object Explore_AnalyzerInventoryDb {

  /** Starting from the beginning of CE. */
  type Year = Int

  /** Value in [1,12] */
  type Month = Int

  /** 1 indexed. */
  type Day = Int

  /** From [0, 52] */
  type Week = Int

  type DateS = String

  def cleanName(s: String): String = s.replaceAll("\"", "")

  def extractYearMonthDay(date: DateS): (Year, Month, Day) = {
    val bits = date.split("-")
    val year = bits.head.toInt
    val month = DataSchemaHelpers.monthNameToNum(bits(1))
    val day = bits(2).toInt
    (year, month, day)
  }

  /** True if the date of t1 is before t2. False otherwise. */
  def trayDateLt(t1: Tray, t2: Tray): Boolean = {
    val (y1, m1, d1) = extractYearMonthDay(t1.date)
    val (y2, m2, d2) = extractYearMonthDay(t2.date)
    if (y1 > y2)
      false
    else if (m1 > m2)
      false
    else if (d1 > d2)
      false
    else
      true
  }

  def trayNameToKeyFragment(name: String): String =
    name.replaceAll("[0-9]*", "")

  def leadingTwoZeros(x: String): String =
    ("00" + x).substring(x.length)

  def trayDateKey(t: Tray) = {
    val (y, m, d) = extractYearMonthDay(t.date)
    new BigInteger(
      y.toString + leadingTwoZeros(m.toString) + leadingTwoZeros(d.toString)
    ).longValueExact()
  }

  /** 0 indexed: First week is 0. Last week is 52. */
  def dateToWeek(s: DateS): Week = {
    import java.util._
    val (y, m, d) = extractYearMonthDay(s)
    val c = new GregorianCalendar()
    c.set(y, m - 1, d)
    c.get(Calendar.WEEK_OF_YEAR) - 1
  }

  def groupByYear(trays: Seq[Tray]): Map[Year, Seq[Tray]] =
    trays
      .foldLeft(Map.empty[Year, Seq[Tray]]) {
        case (a, t) =>
          val (y, _, _) = extractYearMonthDay(t.date)
          if (a.contains(y))
            (a - y) + (y -> (a(y) :+ t))
          else
            a + (y -> List(t))
      }
      .map { case (week, ts) => (week, ts.sortBy { trayDateKey }) }

  def groupByWeek(trays: Seq[Tray]): Map[Week, Seq[Tray]] =
    trays
      .foldLeft(Map.empty[Week, Seq[Tray]]) {
        case (a, t) =>
          val week = dateToWeek(t.date)
          if (a.contains(week))
            (a - week) + (week -> (a(week) :+ t))
          else
            a + (week -> List(t))
      }
      .map { case (week, ts) => (week, ts.sortBy { trayDateKey }) }

  def orgByDate(trays: Seq[Tray]): Map[Year, Map[Week, Seq[Tray]]] =
    groupByYear(trays).foldLeft(Map.empty[Year, Map[Week, Seq[Tray]]]) {
      case (a, (y, traysInYear)) =>
        a + (y -> groupByWeek(traysInYear))
    }

  def trayHistory(all_trays: Seq[Tray]): Map[String, Seq[Tray]] =
    all_trays
      .foldLeft(Map.empty[String, Seq[Tray]]) {
        case (a, tray) =>
          val name = cleanName(tray.name)
          if (a.contains(name)) {
            (a - name) + (name -> (a(name) :+ tray))
          } else {
            a + (name -> List(tray))
          }
      }
//      .map { case (name, trays) => (name, trays.sortWith { trayDateLt }) }
      .map { case (name, trays) => (name, trays.sortBy { trayDateKey }) }

  def traysOf(tray2history: Map[_, Seq[Tray]]): Seq[Tray] =
    tray2history.flatMap { case (_, trays) => trays }.toSeq.sortBy {
      trayDateKey
    }

  type Actual = Int
  type Total = Int
  type Percent = Double

  def compMissing(trays: Seq[Tray]): (Actual, Total, Percent) = {
    val (totalAccount, totalTarget) = trays.foldLeft((0, 0)) {
      case ((aAcct, aTarget), t) =>
        val (act, tar) = t.devices.foldLeft((0, 0)) {
          case ((aa, at), d) => (aa + d.account, at + d.target)
        }
        (aAcct + act, aTarget + tar)
    }
    val percentMissing = (totalAccount.toDouble / totalTarget.toDouble) * 100.0
    (totalAccount, totalTarget, percentMissing)
  }

  def formatTwoDecimal(x: Double): String =
    f"$x%1.2f"

  def printAnalysisMissing(
      byYearByWeekTrays: Map[Year, Map[Week, Seq[Tray]]]
  ): Unit =
    byYearByWeekTrays.foreach {
      case (year, byWeek) =>
        println(s"year: $year")
        println("----------")

        (0 until 52).foreach { week =>
          val weekS = leadingTwoZeros(week.toString)
          if (byWeek.contains(week)) {
            val trays = byWeek(week)
            val (actual, total, percentMissing) = compMissing(trays)
            val pmS = formatTwoDecimal(percentMissing)
            println(
              s"week: $weekS\tmissing_percent: $pmS %\tmissing_actual: $actual")
          } else {
            println(s"week: $weekS\tmissing: no information")
          }
        }
        println("")
    }

  def main(args: Array[String]): Unit = {

    if (args.isEmpty || args.length < 2) {
      System.err.println(
        """Expecting a sequence of 1 or more input files or directories followed by 1 output filepath.""")
      System.exit(1)
    }

    val inputs: IndexedSeq[File] = {

      def deeper(f: File): IndexedSeq[File] =
        if (f.isFile)
          IndexedSeq(f)
        else if (f.isDirectory)
          f.listFiles.toIndexedSeq.flatMap { deeper } //
        else
          IndexedSeq.empty[File]

      args.slice(0, args.length - 1).flatMap { x =>
        deeper(new File(x))
      }
    }

    val output: File = new File(args(args.length - 1))

    System.err.println(
      s"""After expanding the input arguments, there are ${inputs.size} JSON files:
        ${inputs.map { _.toString }.mkString("\n\t")}
      """
    )

    System.err.println(s"Writing analysis to: $output")

    val allTrays =
      inputs.foldLeft(IndexedSeq.empty[Tray]) {
        case (aTrays, fi) =>
          val trays =
            Json.parse(Source.fromFile(fi).mkString).as[IndexedSeq[Tray]]
          println(
            s"$time | Parsed inventory for ${trays.head.date}, has ${trays.size} trays.")
          aTrays ++ trays
      }

    val tray2history = trayHistory(allTrays)
    println(s"${tray2history.size} unique trays")

    val trayType2history = tray2history
      .foldLeft(tray2history.empty) {
        case (a, (tray, history)) =>
          val compactName = trayNameToKeyFragment(tray)
          if (a.contains(compactName)) {
            (a - compactName) + (compactName -> (a(compactName) ++ history))
          } else {
            a + (compactName -> history)
          }
      }
      .map {
        case (compactName, history) =>
          (compactName, history.sortBy { trayDateKey }.toIndexedSeq)
      }
    println(s"${trayType2history.size} tray types")

//    val byYearByWeekTrays = orgByDate(traysOf(tray2history))

    println("")
    println("\n\nBY TRAY NAME\n\n")
    tray2history.foreach {
      case (name, trays) =>
        val byYearByWeekTraysForName = orgByDate(trays)
        println(s"Tray: $name")
        println(s"=====================")
        printAnalysisMissing(byYearByWeekTraysForName)
        println("")

    }
    println("\n\nBY TRAY TYPE\n\n")
    trayType2history.foreach {
      case (name, trays) =>
        val byYearByWeekTraysForName = orgByDate(trays)
        println(s"Tray: $name")
        println(s"=====================")
        printAnalysisMissing(byYearByWeekTraysForName)
        println("")
    }
    println("\n\nALL TRAYS\n\n")
    println(s"=====================")
    printAnalysisMissing(orgByDate(traysOf(tray2history)))

//    val targetNameFragment = "BGEN"
//    val targetedTrays: IndexedSeq[Tray] = trayType2history(targetNameFragment)
//    println(
//      s"${targetedTrays.size} trays that have $targetNameFragment in their name")
//
//    import sutils.fp.ImplicitDisjunctionOps._
//    JsonSupport.writeTrays(output, targetedTrays).getUnsafe
//
//    val (totalAccount, totalTarget, percentMissing) = compMissing(targetedTrays)
//    println(
//      s"For the $targetNameFragment trays, expecting $totalTarget but actually have $totalAccount tools (${formatTwoDecimal(percentMissing)} % missing)"
//    )

  }

}

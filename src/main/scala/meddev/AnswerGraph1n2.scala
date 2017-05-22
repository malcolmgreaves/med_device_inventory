package meddev

import java.io._
import java.math.BigInteger

import cmd.RunnerHelpers.time
import rapture.json._
import rapture.json.jsonBackends.jawn._

import scala.io.Source

object AnswerGraph1n2 {

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

  def groupByMonth(trays: Seq[Tray]): Map[Month, Seq[Tray]] =
    trays.foldLeft(Map.empty[Month, Seq[Tray]]) {
      case (a, t) =>
        val (_, month, _) = extractYearMonthDay(t.date)
        if (a.contains(month))
          (a - month) + (month -> (a(month) :+ t))
        else
          a + (month -> Seq(t))
    }

  def orgByDate(trays: Seq[Tray]): Map[Year, Map[Week, Seq[Tray]]] =
    groupByYear(trays).foldLeft(Map.empty[Year, Map[Week, Seq[Tray]]]) {
      case (a, (y, traysInYear)) =>
        a + (y -> groupByWeek(traysInYear))
    }

  case class UniqueTray(name: String, serialNum: String) {
    override def toString = s"$name-$serialNum"
  }

  def trayToUnique(t: Tray): UniqueTray =
    UniqueTray(name = cleanName(t.name), serialNum = cleanName(t.serialNum))

  def trayHistory(allTrays: Seq[Tray]): Map[UniqueTray, Seq[Tray]] =
    allTrays
      .foldLeft(Map.empty[UniqueTray, Seq[Tray]]) {
        case (a, tray) =>
          val uniqueTray = trayToUnique(tray)
          if (a.contains(uniqueTray))
            (a - uniqueTray) + (uniqueTray -> (a(uniqueTray) :+ tray))
          else
            a + (uniqueTray -> List(tray))
      }
      .map { case (name, trays) => (name, trays.sortBy { trayDateKey }) }

  def analysis_missing_by_unique_tray_total(trays: Seq[Tray]): Unit = {
    trayHistory(trays).foreach {
      case (unique, _trays) =>
    }
  }

  def trayTypeToHistory(allTrays: Seq[Tray]): Map[String, Seq[Tray]] =
    allTrays
      .foldLeft(Map.empty[String, Seq[Tray]]) {
        case (a, tray) =>
          val name = cleanName(tray.name)
          if (a.contains(name))
            (a - name) + (name -> (a(name) :+ tray))
          else
            a + (name -> List(tray))
      }
      .map { case (name, trays) => (name, trays.sortBy { trayDateKey }) }
      .foldLeft(Map.empty[String, Seq[Tray]]) {
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

  def organize[K: Ordering, T](m: Map[K, T]): Seq[(K, T)] =
    m.toSeq.sortBy { case (key, _) => key }

  val specialTrayNames = Set(
    "WORA002",
    "WORA011",
    "WORA032",
    "WORA032",
    "WNEU063",
    "WNEU247",
    "EPLA001",
    "EORT322",
    "BGEN018"
  )

  val TRAY_1 = "WENT160"
  val TRAY_2 = "WENT024"
  val TRAY_3 = "WENT084"
  val TRAY_4 = "WENT155"

  def selectTrays1(trays: Seq[Tray]): Seq[Tray] =
    trays.filter { tray =>
      tray.name.equals(TRAY_1)
    }

  def selectTrays2(trays: Seq[Tray]): Seq[Tray] =
    trays.filter { tray =>
      tray.name.equals(TRAY_2)
    }

  def selectTrays3(trays: Seq[Tray]): Seq[Tray] =
    trays.filter { tray =>
      tray.name.equals(TRAY_3)
    }

  def selectTrays4(trays: Seq[Tray]): Seq[Tray] =
    trays.filter { tray =>
      tray.name.equals(TRAY_4)
    }

  def main(args: Array[String]): Unit = {
    println(s"Executing (class): ${this.getClass.getName}\n")

    val N_REQ = 1

    if (args.isEmpty || args.length < N_REQ) {
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

      args.slice(0, math.max(args.length - 1, 1)).flatMap { x =>
        System.err.println(s"Reading file: $x")
        deeper(new File(x))
      }
    }

//    val output: File = new File(args(args.length - 1))

    System.err.println(
      s"""After expanding the input arguments, there are ${inputs.size} JSON files:
        ${inputs.map { _.toString }.mkString("\n\t")}
      """
    )

//    System.err.println(s"Writing analysis to: $output")

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

    tray2history.keys.slice(0, 5).foreach { println }

    val trayType2history = trayTypeToHistory(allTrays)
    println(s"${trayType2history.size} tray types\n")

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    val targetTrays = get_special_went_trays(trayType2history, allTrays)

    print("")
    print_graph_1_and_2_analysis(allTrays, targetTrays)

    println("")
    print_all_days_record(allTrays)

    print("")
    print_graph4_analysis(allTrays)
  }

  def print_graph_1_and_2_analysis(allTrays: Seq[Tray],
                                   targetTrays: Seq[Tray]): Unit = {
    print(s"\n2015\n")
    print_analysis_year(
      special = targetTrays,
      all = allTrays,
      year = 2015
    )

    print(s"\n2016\n")
    print_analysis_year(
      special = targetTrays,
      all = allTrays,
      year = 2016
    )
  }

  def print_graph4_analysis(allTrays: Seq[Tray]): Unit = {
    print("\nGRAPH 4\n")

    print("4-tray intervention analysis, by month:")
    print("\n2015")
    print_analysis_graph_4_by_month(allTrays, 2015)
    print("\n2016")
    print_analysis_graph_4_by_month(allTrays, 2016)

//    print("4-tray intervention analysis, by discrete date from data:")
//    print("\n2015")
//    print_analysis_graph_4_discrete_date(allTrays, 2015)
//    print("\n2016")
//    print_analysis_graph_4_discrete_date(allTrays, 2016)
  }

  def print_analysis_graph_4_by_month(allTrays: Seq[Tray], year: Year): Unit = {

    println(s"Instruments missing from $TRAY_1 in $year")
    analysis_total_missing_instruments_monthly(
      inGivenYear(selectTrays1(allTrays), year))
    println("")

    println(s"Instruments missing from $TRAY_2 in $year")
    analysis_total_missing_instruments_monthly(
      inGivenYear(selectTrays2(allTrays), year))
    println("")

    println(s"Instruments missing from $TRAY_3 in $year")
    analysis_total_missing_instruments_monthly(
      inGivenYear(selectTrays3(allTrays), year))
    println("")

    println(s"Instruments missing from $TRAY_4 in $year")
    analysis_total_missing_instruments_monthly(
      inGivenYear(selectTrays4(allTrays), year))
    println("")
  }

  def print_analysis_graph_4_discrete_date(allTrays: Seq[Tray],
                                           year: Year): Unit = {
    ???
  }

  def print_all_days_record(allTrays: Seq[Tray]): Unit = {
    println("Number of days of recorded data, per month, per year\n")
    println("2015")
    print_days_record_per_month(inGivenYear(allTrays, 2015))
    println("2016")
    print_days_record_per_month(inGivenYear(allTrays, 2016))
  }

  def print_days_record_per_month(trays: Seq[Tray]): Unit = {
    val x = organize(days_recorded_per_month(trays))
    x.foreach {
      case (month, n) =>
        val m = DataSchemaHelpers.monthNumToName(month)
        println(s"$m,$n")
    }
    println("")
  }

  def days_recorded_per_month(trays: Seq[Tray]): Map[Month, Int] =
    groupByMonth(trays).map {
      case (month, mTrays) =>
        val x = mTrays
          .foldLeft(Set.empty[Day]) {
            case (a, tray) =>
              val (_, _, day) = extractYearMonthDay(tray.date)
              a + day
          }
          .size
        (month, x)
    }

  def print_analysis_year(special: Seq[Tray],
                          all: Seq[Tray],
                          year: Year): Unit = {

    println(s"\n$year REPORTED TRAYS (totals)\n")

    println(s"Number of special & WENT* trays missing in $year")
    analysis_raw_tray_counts(inGivenYear(special, year))

    println(s"Number of ALL trays missing in $year")
    analysis_raw_tray_counts(inGivenYear(all, year))

    println(s"\n$year MISSING INSTRUMENTS\n")

    println(
      s"Number of instruments missing from special & WENT trays in $year")
    analysis_total_missing_instruments_monthly(inGivenYear(special, year))

    println(s"Number of instruments missing from all trays in $year")
    analysis_total_missing_instruments_monthly(inGivenYear(all, year))
  }

  def inGivenYear(ts: Seq[Tray], year: Year): Seq[Tray] =
    ts.filter { tray =>
      val (y, _, _) = extractYearMonthDay(tray.date)
      y == year
    }

  def get_special_went_trays(trayType2history: Map[String, Seq[Tray]],
                             allTrays: Seq[Tray]): Seq[Tray] = {

    val wentTrays = trayType2history("WENT")
    println(s"There are ${wentTrays.size} WENT trays")

    val specialTrays = allTrays.filter { tray =>
      specialTrayNames.contains(tray.name)
    }
    println(
      s"There are ${specialTrays.size} trays from one of the ${specialTrayNames.size} special tray names")

    println("")
    wentTrays ++ specialTrays
  }

  def analysis_raw_tray_counts(targetTrays: Seq[Tray]): Unit =
    print_by_month_X(
      targetTrays,
      "number of trays",
      (trays: Seq[Tray]) => trays.size.toString
    )

  def analysis_total_missing_instruments_monthly(
      targetTrays: Seq[Tray]): Unit =
    print_by_month_X(
      targetTrays,
      "number of missing items",
      (trays: Seq[Tray]) => {
        val (missing, totalExpected, _) = compMissing(trays)
        missing.toString
      }
    )

  def print_by_month_X(trays: Seq[Tray],
                       name: String,
                       dataSelect: Seq[Tray] => String): Unit = {
    println(s"month,$name")
    val traysByMonth = organize(groupByMonth(trays))
    traysByMonth.foreach {
      case (month, _trays) =>
        val monthStr = DataSchemaHelpers.monthNumToName(month)
        println(s"$monthStr,${dataSelect(_trays)}")
    }
    println("")
  }

}

package meddev

import java.io._
import java.time.format.DateTimeFormatter
import java.util.Date

import scala.io.Source
import scala.util.control.Breaks._
import cmd.RunnerHelpers.time

import scalaz.\/
import rapture.json._
import jsonBackends.jawn._
import sutils.fp.Types.Err

case class Header(nCols: Int,
                  position: Index,
                  target: Index,
                  account: Index,
                  number: Index,
                  item: Index,
                  name: Index,
                  repair: Index,
                  originalText: String)

case class Tray(date: String,
                name: String,
                serialNum: String,
                // notes: String,
                devices: IndexedSeq[Device],
                originalText: String)

object Tray {
  val empty: Tray = Tray(null, "", "", IndexedSeq.empty[Device], "")
}

case class Device(position: Int,
                  target: Int,
                  account: Int,
                  number: String,
                  item: String,
                  name: String,
                  repair: String,
                  originalText: String)

object Device {
  val empty: Device =
    Device(Integer.MIN_VALUE,
           Integer.MIN_VALUE,
           Integer.MIN_VALUE,
           "",
           "",
           "",
           "",
           "")
}

case class MedicalDeviceInventory(date: String,
                                  header: Header,
                                  trays: IndexedSeq[Tray])

object ParserDeviceInventoryCsv {

  private def nSepToSN(bits: Array[String]): Int = {
    var i = 0
    breakable {
      while (i < bits.length) {

        val x = bits(i)
        if (x.contains("S/N:"))
          break

        i += 1
      }
    }
    i
  }

  def nSepToSerialNumber(fileContents: IndexedSeq[String]): Int = {
    var returnValue = 0
    breakable {
      fileContents.foreach { line =>
        val bits = line.split(",")
        if (bits.nonEmpty && bits.head.nonEmpty) {
          val n = nSepToSN(bits)
          if (n > 0) {
            returnValue = n
            break
          }
        }
      }
    }
    returnValue
  }

  def notesFromLine(bits: Array[String]): String =
    bits(bits.length - 6)
//
//  private val dateFormats = List(
//    "MM/DD/uuuu"
////    "dd/MM/uuuu",
////    "MMM dd, uuuu",
////    "dd MMMM uuuu",
////    "dd MMM uuuu",
////    "dd-MM-uuuu"
//  ).map(p => (p, DateTimeFormatter.ofPattern(p)))
//  private val iso8601DateFormatter = DateTimeFormatter.ISO_LOCAL_DATE
//
//  def dateParse(dateStr: String): Option[String] = {
//    val trimmedDate = dateStr.trim
//    if (trimmedDate.isEmpty)
//      None
//    else
//      dateFormats.toStream.flatMap {
//        case (pattern, fmt) =>
//          println(s"Pattern: $pattern")
//          \/.fromTryCatchNonFatal { fmt.parse(trimmedDate) }.toOption
//      }.map { iso8601DateFormatter.format }.headOption
//  }

  def dateParse(s: String): Option[String] =
    \/.fromTryCatchNonFatal {
      val bits = s.split("""/""")
      if (bits.length > 3)
        throw new Exception("")

      val month = bits(0).toInt match {
        case 1 => "Jan"
        case 2 => "Feb"
        case 3 => "Mar"
        case 4 => "Apr"
        case 5 => "May"
        case 6 => "Jun"
        case 7 => "Jul"
        case 8 => "Aug"
        case 9 => "Sep"
        case 10 => "Oct"
        case 11 => "Nov"
        case 12 => "Dec"
      }
      val day = bits(1).toInt
      val year = bits(2).toInt

      s"$year-$month-$day"
    }.toOption

  def findHeaderInfo(fileContents: IndexedSeq[String])
    : (Header, String, IndexedSeq[String]) = {

    val date = {
      val i = fileContents.indexWhere {
        _.contains("Froedtert Memorial Lutheran Hospital")
      }
      val bits = fileContents(i).split(",")
      bits.view.reverse.flatMap { dateParse }.head
    }

    val indexOfHeader = fileContents.indexWhere { line =>
      line.contains("Pos") && line.contains("Tgt") && line.contains("Act")
    }

    val header = {
      val headerStr = fileContents(indexOfHeader)
      val bits = headerStr.split(",")
      Header(nCols = bits.length,
             position = Index(bits.indexWhere { _.contains("Pos") }),
             target = Index(bits.indexWhere { _.contains("Tgt") }),
             account = Index(bits.indexWhere { _.contains("Act") }),
             number = Index(bits.indexWhere { _.contains("No") }),
             item = Index(bits.indexWhere { _.contains("Item") }),
             name = Index(bits.indexWhere { _.contains("Name") }),
             repair = Index(bits.indexWhere { _.contains("Repair") }),
             originalText = headerStr)
    }

    val remaining = fileContents.slice(indexOfHeader + 1, fileContents.length)

    (header, date, remaining)
  }

  def parse(fileContents: IndexedSeq[String]): MedicalDeviceInventory = {

    val (header, date, remaining) = findHeaderInfo(fileContents)

    println(s"HEADER:\n$header")
    println(s"DATE:\n$date")

    /// ,Pos,,Tgt.,,Act.,,,No.,,,Item No.,,,,,,Name,,,,Repair

    val nSepToSerialNumberValue = {
      val nSeparatorsToSerialNumberReference = nSepToSerialNumber(remaining)
      nSeparatorsToSerialNumberReference + 2
    }

    val trays: IndexedSeq[Tray] = {
      val (aTrays, leftoverTray) =
        remaining.foldLeft((IndexedSeq.empty[Tray], Option.empty[Tray])) {
          case ((accumTrays, maybeTray), line) =>
            val bits = line.split(",")

            if (bits.forall(_.isEmpty)) {
              (accumTrays, maybeTray)

            } else if (bits.head.nonEmpty) {
              // STARTING A NEW TRAY

              val newTray = Tray(
                date = date,
                name = bits.head,
                serialNum = bits(nSepToSerialNumberValue),
//                notes = notesFromLine(bits),
                devices = IndexedSeq.empty[Device],
                originalText = line
              )

              (
                maybeTray.fold(accumTrays)(t => accumTrays :+ t),
                Some(newTray)
              )

            } else if (line.contains("S/N:")) {
              // TODO
              // FIXME
              val serialNumber = bits(nSepToSerialNumberValue)
              val notes = notesFromLine(bits)

              System.err.println(
                s"WHAT DO TO! I found a serial number ($serialNumber) and notes ($notes) -- what does this format mean?: $line") //

              (accumTrays, maybeTray)

            } else {
              // ADDING A NEW DEVICE TO AN EXISTING TRAY

              val newDevice = Device(
                position = bits(Index(header.position)).toInt,
                target = bits(Index(header.target)).toInt,
                account = bits(Index(header.account)).toInt,
                number = bits(Index(header.number)),
                item = bits(Index(header.item)),
                name = bits(Index(header.name)),
                repair = \/.fromTryCatchNonFatal { bits(Index(header.repair)) }
                  .getOrElse(""),
                originalText = line
              )

              if (maybeTray.isEmpty)
                throw new IllegalStateException(
                  s"Bad CSV format: expecting to add a device to a non-existant tray. Device in question:\n$newDevice")

              (
                accumTrays,
                maybeTray.map { tray =>
                  tray.copy(devices = tray.devices :+ newDevice)
                }
              )
            }
        }

      leftoverTray.fold(aTrays) { t =>
        aTrays :+ t
      }
    }

    MedicalDeviceInventory(date, header, trays)
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
      s"""After expanding the input arguments, there are ${inputs.size} csv files:
        ${inputs.map { _.toString }.mkString("\n\t")}
      """
    )

    System.err.println(
      s"Writing all cleaned and extracted information in JSON format to: $output")

    val allInventories =
      inputs.foldLeft(IndexedSeq.empty[MedicalDeviceInventory]) {
        case (inventories, fi) =>
          val inventory = parse(Source.fromFile(fi).getLines.toIndexedSeq)
          println(
            s"$time | Parsed inventory for ${inventory.date}, has ${inventory.trays.size} trays.")
          inventories :+ inventory
      }

    val w = new BufferedWriter(new FileWriter(output))
    val j = Json(allInventories.flatMap { _.trays })
    import formatters.compact._
    w.write(Json.format(j))
    w.close()
  }

}

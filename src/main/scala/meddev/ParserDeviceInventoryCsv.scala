package meddev

import java.io._

import scala.io.Source
import scala.util.control.Breaks._
import cmd.RunnerHelpers.time

import scalaz.\/
import rapture.json._
import jsonBackends.jawn._

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

  def dateParse(s: String): Option[String] =
    \/.fromTryCatchNonFatal {
      val bits = s.split("""/""")
      if (bits.length > 3)
        throw new Exception("")

      val month = DataSchemaHelpers.monthNumToName(bits(0).toInt)
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
      val (aTrays, leftoverTray, _) =
        remaining.foldLeft((IndexedSeq.empty[Tray], Option.empty[Tray], "")) {
          case ((accumTrays, maybeTray, currSerialNum), line) =>
            val bits = line.split(",")

            if (bits.forall(_.isEmpty)) {
              (accumTrays, maybeTray, currSerialNum)

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
                Some(newTray),
                newTray.serialNum
              )

            } else if (line.contains("S/N:")) {

              val serialNumber = bits(nSepToSerialNumberValue)
              val notes = notesFromLine(bits)

              (accumTrays, maybeTray, serialNumber)

            } else {
              // ADDING A NEW DEVICE TO AN EXISTING TRAY

              val newDevice = Device(
                position = bits(Index(header.position)).toInt,
                target = bits(Index(header.target)).toInt,
                account = bits(Index(header.account)).toInt,
                number = bits(Index(header.number)),
                item = bits(Index(header.item)),
                serialNum = currSerialNum,
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
                },
                currSerialNum
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

    println(
      s"""After expanding the input arguments, there are ${inputs.size} csv files:
        ${inputs.map { _.toString }.mkString("\n\t")}
      """
    )

    println(
      s"Writing all cleaned and extracted information in JSON format to: $output")

    case class EF(fi: File, err: Throwable)

    val (allInventories, allErrFis) =
      inputs.foldLeft(
        (IndexedSeq.empty[MedicalDeviceInventory], List.empty[EF])) {
        case ((inventories, errFis), fi) =>
          \/.fromTryCatchNonFatal {
            val inventory =
              parse(Source.fromFile(fi).getLines.toSeq.toIndexedSeq)
            println(
              s"$time | Parsed inventory for ${inventory.date}, has ${inventory.trays.size} trays.")
            (inventories :+ inventory, errFis)

          }.fold(err => (inventories, errFis :+ EF(fi, err)), identity)
      }

    if (allErrFis.nonEmpty) {
      System.err.println(
        s"Failed to process ${allErrFis.size} CSV files. These were:")
      val em = allErrFis.zipWithIndex.map {
        case (EF(fi, err), i) =>
          s"""\t[${i + 1} / ${allErrFis.size}] $fi\n\t----\n\t${err.getStackTrace
            .mkString("\n\t")}"""
      }.mkString("\n\n")
      System.err.println(em)
    }

    println(
      s"\nSuccessfully parsed ${allInventories.size} / ${inputs.size} CSV files")

    import sutils.fp.ImplicitDisjunctionOps._
    JsonSupport
      .writeTrays(output, allInventories.flatMap { _.trays })
      .getUnsafe
  }

}

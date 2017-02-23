package meddev

import java.io.{BufferedWriter, File, FileWriter}

import sutils.fp.Types.SideEffect

import scalaz.\/

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
                  serialNum: String,
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
           "",
           "")
}

case class MedicalDeviceInventory(date: String,
                                  header: Header,
                                  trays: IndexedSeq[Tray])

object JsonSupport {

  import rapture.json._
  import jsonBackends.jawn._
  import formatters.compact._

  def writeTrays(out: File, trays: IndexedSeq[Tray]): SideEffect =
    \/.fromTryCatchNonFatal {
      val w = new BufferedWriter(new FileWriter(out))
      val j = Json(trays)
      import formatters.compact._
      w.write(Json.format(j))
      w.close()
    }

}

object DataSchemaHelpers {

  def monthNameToNum(s: String): Int = s match {
    case "Jan" => 1
    case "Feb" => 2
    case "Mar" => 3
    case "Apr" => 4
    case "May" => 5
    case "Jun" => 6
    case "Jul" => 7
    case "Aug" => 8
    case "Sep" => 9
    case "Oct" => 10
    case "Nov" => 11
    case "Dec" => 12
  }

  def monthNumToName(s: Int): String = s match {
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

}

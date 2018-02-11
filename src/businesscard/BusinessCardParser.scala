package businesscard

import java.util.Properties
import java.util.regex.Pattern
import java.io._
import org.apache.log4j.Logger

object BusinessCardParser {
  val logger = Logger.getLogger(classOf[ContactInfo])
  
  /**
   * The Business Card properties hold resources needed to parse a busness card document
   * 
   * The properties is initialized with default properties values but allows loading of user defined properties
   */
  val NAME_FILE_PROPERTY = "businesscard.name.file"
  
  var properties = {
    val props = new Properties()
    props.setProperty(NAME_FILE_PROPERTY, "resources/names.txt")
    props
  }
  
  def loadProperties(file : String) = try {
    
  } catch {
    case ex : FileNotFoundException =>
      logger.error(s"Properties file $file not found")
    case ex : IOException =>
      logger.error(s"Error reading Properties file $file")
  }
  
  /**
   * A list of names used in parsing the name line from the document
   */
  lazy val names : Set[String] = try {
      val io = new BufferedReader(new InputStreamReader(new FileInputStream(properties.getProperty(NAME_FILE_PROPERTY))))
      def readNames(accum : Set[String]) : Set[String] = {
        val name = io.readLine()
        if (null == name)
          accum
        else
          readNames(accum + name.toLowerCase)
      }
      
      readNames(Set[String]())
    } catch {
      case ex : Exception => 
        logger.error("Error loading names file")
        null
    }
  
  /**
   * Defined types that represent the contents of a scanned Business Card
   */
  type Line = List[String]
  type Document = List[Line]

  /**
   * the default parseName function returns the line that contains the highest perportion
   * of words that are contained in the set of known names.  This should in most discriminate 
   * between the actual name line and a company name line that contains a person's name as part of the company name.
   */
  def defaultParseName(document : Document) : String = {
    def calcName(doc : Document, cumScore : Float, curName : String) : String = {
      if (doc == Nil)
        curName
      else {
        val lineScore = (1.0f * doc.head.filter (word => names.contains(word.toLowerCase)).size)/ (1.0f * doc.head.size)
        val newScore = if (lineScore > cumScore) lineScore else cumScore
        val newName = if (lineScore > cumScore) doc.head.mkString(" ") else curName
        calcName(doc.tail, newScore, newName)
      }
    }
    
    calcName(document, 0.0f, "unknown")
  }
  
  /**
   * the default parsePhoneNumber uses a simple algorithm to find the phone number.
   * it just chooses the first line with a minimum of 9 numeric characters and strips out all 
   * non-numeric characters.  A more elaborate function my try to use a regex pattern to discover
   * a phone number and possibly use key words like phone, cell or (excluding) fax
   */
  def defaultParsePhoneNumber(document : Document) : String = {
    def getNumber(line : Line) : String = line.mkString("").toCharArray.filter(ch => ch >= '0' && ch <= '9').mkString("")
    def getPhoneNumberLine(doc : Document) : String = {
      if (doc == Nil)
        "unknown"
      else if (getNumber(doc.head).length >= 9)
        getNumber(doc.head)
      else
        getPhoneNumberLine(doc.tail)
    }
        
    getPhoneNumberLine(document)
  }
  
  /**
   * the default paresEmailAddress returns the first word in the document that matches
   * the valid email address regex pattern
   */
  val EMAIL_ADDRESS_PATTERN = Pattern.compile("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$", Pattern.CASE_INSENSITIVE)
  def defaultParseEmailAddress(document : Document) : String = {
    val emailAddressList = document flatMap {line => line} filter {word => EMAIL_ADDRESS_PATTERN.matcher(word).find}
    if (emailAddressList != Nil) emailAddressList.head else "unknown"
  }
  
  /**
   * getContactInfo returns an instance of ContactInfo for a scanned business card text document
   * if called as getContactInfor(document) it will use the default parser functions to parse the
   * name, phone number and email address. To override the defaults call:
   * getContactInfo(yourParesName, yourPhoneNumber, yourParesEmailAddress)(document)
   */
  def getContactInfo(cardText : String) : ContactInfo = getContactInfo(defaultParseName, defaultParsePhoneNumber, defaultParseEmailAddress)(cardText)
  def getContactInfo(
        parseName : Document => String,
        parsePhoneNumber : Document => String,
        parseEmailAddress : Document => String 
      )(cardText : String) : ContactInfo = {
    val document = cardText.trim.split("\\r?\\n").toList.map {_.trim.split("\\s+").toList}
    
    ContactInfo(parseName(document), parsePhoneNumber(document), parseEmailAddress(document))
  }
}
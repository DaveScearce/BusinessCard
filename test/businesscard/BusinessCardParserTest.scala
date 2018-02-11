package businesscard

import java.io._
import org.scalatest.FlatSpec

class BusinessCardParserTest extends FlatSpec {
  
  def readDocument(fileName : String) : String = {
    val io = new BufferedReader(new InputStreamReader(new FileInputStream(fileName)))
    def readDocs(doc : String) : String = {
      val line = io.readLine()
      if (line == null)
        doc
      else 
        readDocs(doc + "\n" + line)
    }
    
    readDocs(io.readLine)
  }
  
  "Document Example 1" should "should yield the correct info" in {
    val document = readDocument("resources/card1.txt")
    val contactInfo = BusinessCardParser.getContactInfo(document)
    assert(contactInfo.getName === "Mike Smith")
    assert(contactInfo.getPhoneNumber == "4105551234")
    assert(contactInfo.getEmailAddress == "msmith@asymmetrik.com")
  }
  
  "Document Example 2" should "should yield the correct info" in {
    val document = readDocument("resources/card2.txt")
    val contactInfo = BusinessCardParser.getContactInfo(document)
    assert(contactInfo.getName === "Lisa Haung")
    assert(contactInfo.getPhoneNumber == "4105551234")
    assert(contactInfo.getEmailAddress == "lisa.haung@foobartech.com")
  }
  
  "Document Example 3" should "should yield the correct info" in {
    val document = readDocument("resources/card3.txt")
    val contactInfo = BusinessCardParser.getContactInfo(document)
    assert(contactInfo.getName === "Arthur Wilson")
    assert(contactInfo.getPhoneNumber == "17035551259")
    assert(contactInfo.getEmailAddress == "awilson@abctech.com")
  }

}
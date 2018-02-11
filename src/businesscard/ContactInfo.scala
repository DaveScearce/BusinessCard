package businesscard

case class ContactInfo(name : String, phoneNumber : String, emailAddress : String) {
  def getName() = name
  def getPhoneNumber() = phoneNumber
  def getEmailAddress() = emailAddress
}
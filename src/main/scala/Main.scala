import scala.util.matching.Regex

object retune {
  
  implicit class OfSyntax(n: Int) {
    def of(s: String): Regex =
      s"$s{$n}".r
    
    def of(r: Regex): Regex =
      s"${r.regex}{$n}".r
  }


  /** Concat two Scala regexes */
  implicit class RegexSyntax(leftRegex: Regex) {
    def ::(rightRegex: Regex): Regex = {
      println(leftRegex)
      println(rightRegex)
      (leftRegex.regex ++ rightRegex.regex).r
    }
    
    def ::(rightRegex: String): Regex = 
      (leftRegex.regex ++ rightRegex).r

    def as(groupName: Symbol): Regex = 
      s"(?<${groupName.name}> ${leftRegex.regex})".r
  }

  implicit class RegexStringSyntax(leftRegex: String) {
    def ::(rightRegex: Regex): Regex = 
      (leftRegex ++ rightRegex.regex).r
    
    def ::(rightRegex: String): Regex = 
      (leftRegex ++ rightRegex).r
  }



  val word = raw"\w".r
  val digit = raw"\d".r
  val start = "^".r
  val end = "$".r
  def capture(reg: Regex): Regex = s"(${reg.regex})".r

  
  val eth_regex1 = "^((en[pxos]).*|eth.*)".r
  //val eth_regex2 = start ++ capture (capture () ++ (any of ".") ++ )


  // "en" ++ one of ("p", "x", "o", "s") ++ any of "eth" ++ 

  val date = raw"(\d{4})-(\d{2})-(\d{2})".r
  val dateP: Regex = capture (4 of digit) :: "-" :: capture (2 of digit) :: "-" :: capture (2 of digit)

  "01-20-2004" match {
    case dateP(month, day, year) => println(s"$year was a good year for PLs.")
  }

  println(date)
  println(dateP)

  val keyValPattern: Regex = "([0-9a-zA-Z- ]+): ([0-9a-zA-Z-#()/. ]+)".r

  val reg = start :: (2 of "na") :: end

  reg.findFirstMatchIn("lalananalal") match {
    case Some(_) => println("Password OK")
    case None => println("Password must contain a number")
  }
}


object Main extends App {
  import retune._

  val date = raw"(\d{4})-(\d{2})-(\d{2})".r
  val dateP = ((4 of digit) as 'year) :: "-" :: (2 of digit) :: "-" :: capture (2 of digit)

  val pattern = "([0-9]+) ([A-Za-z]+)".r
  val patt = (some of digit) :: " " :: (some of letter)

  println(date)
  println(dateP)

  "01-20-2004" match {
    case dateP(month, day, year) => println(s"$year was a good year for PLs.")
    case _ => println("fail")
  }
}
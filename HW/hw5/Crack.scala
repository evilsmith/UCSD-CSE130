import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import Crypt._
import java.io.PrintWriter

case class Entry ( account   : String
                 , password  : String
                 , uid       : Int
                 , gid       : Int
                 , gecos     : String
                 , directory : String
                 , shell     : String
                 )

object Entry {
  def apply(line: String) : Entry = {
    val vl = line.split(":")
    Entry(vl(0), vl(1), vl(2).toInt, vl(3).toInt, vl(4), vl(5), vl(6))
  }  
}

object Crack {

  def transformReverse(w: String) : Iterator[String] = {
    List(w, w.reverse).toIterator
  }
  
  def transformCapitalize(w: String) : Iterator[String] = {
    if (w == "") { 
      List(w).toIterator 
    } else {
      for(c <- List(w.substring(0,1).toLowerCase, 
          w.substring(0,1).toUpperCase).toIterator;
          s <- transformCapitalize(w.substring(1)))
            yield (c+s)
    }
  }
  
  def matchLetters(w:String):List[String] = {
    w match {
      case "o" => List("o", "0")
      case "O" => List("O", "0")
      case "z" => List("z", "2")
      case "Z" => List("Z", "2")
      case "a" => List("a", "4")
      case "A" => List("A", "4")
      case "b" => List("b", "6", "8")
      case "B" => List("B", "6", "8")
      case "e" => List("e", "3")
      case "E" => List("E", "3")
      case "g" => List("g", "9")
      case "G" => List("G", "9")
      case "i" => List("i", "1")
      case "I" => List("I", "1")
      case "l" => List("l", "1")
      case "L" => List("L", "1")
      case "q" => List("q", "9")
      case "Q" => List("Q", "9")
      case "s" => List("s", "5")
      case "S" => List("S", "5")
      case "t" => List("t", "7")
      case "T" => List("T", "7")
      case _   => List(w)
    }
  }

  def transformDigits(w:String) : Iterator[String] = {
    if(w == "") {
      List(w).toIterator
    } else {
      for(x <- matchLetters(w.substring(0, 1)).toIterator;
          y <- transformDigits(w.substring(1)))
          yield(x + y)
    }
  }

  def checkPassword(plain: String, enc: String) : Boolean = 
    Crypt.crypt(enc, plain) == enc
 
  def candidateWords(file: String) =
    Words.wordsMatchingRegexp(file, new Regex("""^.{6,8}$"""))

  // scala> Crack("passwd", "words", "soln.1")
  // goto> scala Crack passwd words soln.1
  def apply(pwdFile: String, wordsFile: String, outFile: String) : Unit = {
    var theFile = candidateWords(wordsFile)

    val f1 = new java.io.File(pwdFile)
    val f2 = new java.io.File(wordsFile)
    val out = new java.io.PrintWriter(new java.io.File(outFile))
    try{
      val pwd = scala.io.Source.fromFile(f1).getLines()
      val lines = (for(element <- pwd) yield Entry.apply(element))
      var notThere = true

      for(element <- lines) {
        notThere = true
        for(tmp <- theFile){
          if(checkPassword(tmp, element.password)){
            out.write(element.account + "=" + tmp + "\n")
            out.flush()
            notThere = false
          }

          if(notThere){
            for(t <- transformReverse(tmp)){
              if(notThere && checkPassword(t, element.password))
                out.write(element.account + "=" + tmp + "\n")
                out.flush()
                notThere = false
            }
            if(notThere){
              for(t <- transformCapitalize(tmp)){
                out.write(element.account + "=" + tmp + "\n")
                out.flush()
                notThere = false
              }
              if(notThere){
                for(t <- transformDigits(tmp)){
                  if(notThere && checkPassword(t, element.password))
                    out.write(element.account + "=" + tmp + "\n")
                    out.flush()
                    notThere = false
                }
              }
            }
          }
          }
          theFile = candidateWords(wordsFile)
        }
        out.close()
      } catch {
        case e: java.io.FileNotFoundException => {
          println("NOOOOO!!!!!")
          Iterator()
        }
      }
  }
  
  def main(args: Array[String]) = { 
    println("Begin: Cracking Passwords")
    apply(args(0), args(1), args(2))
    println("Done: Cracking Passwords")
  }
}

// vim: set ts=2 sw=2 et:


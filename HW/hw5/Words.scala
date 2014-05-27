import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import java.io.PrintWriter
import Lines._

object Words {

  def apply(file: String) : Iterator[String] =  
    {
      val f = new java.io.File(file)
      try{
        val lines = scala.io.Source.fromFile(f).getLines()
        for(w <- lines)
          yield w.toLowerCase
      } catch {
        case e: java.io.FileNotFoundException => {
          println("AUUUUUGH!!!! File not found. Please enter a valid file.")
          Iterator()
        }
      }
    } 
  
  def groupFreq[A,B](xs: Iterator[A], f: A => B): HashMap[B, Int] = 
  {
      var map: HashMap[B, Int] = HashMap()
      for(x <- xs){
        if(!map.contains(f(x)))
          map += (f(x) -> 1)
        else
          map += (f(x) -> (map(f(x)) + 1))
      }
      map
  } 


  val inti = List(1,21,5,39,12,7,92)

  def isEven(x: Int): String =
    if ((x % 2) == 0) { "Even" } else { "Odd" } 

  def sizeFreq(file: String): HashMap[Int, Int] = 
  {
    val f = new java.io.File(file)
    val lines = scala.io.Source.fromFile(f).getLines()
    groupFreq(lines, (x:String) => x.size)
  }
  
  def charFreq(file: String): HashMap[Char, Int] = 
  {
    val thing = apply(file)
    val chars   = for(e1 <- thing; c <- e1) yield c
    val grouper = (x: Char) => x
    groupFreq(chars, grouper) 
  }

  def wordsOfSize(file: String, size: Int) : Iterator[String] = 
  {
    val thing = apply(file)
    thing.filter(f => f.size == size)
  }

  def wordsWithAllVowels(file: String): Iterator[String] = 
  {
    val vows = "aeiou"
    val thing = apply(file)
    thing.filter(f => (vows.filter(vo => (!f.contains(vo))).isEmpty))
  }
 
  def wordsWithNoVowels(file: String): Iterator[String] = 
  {
    val vows = "aeiou"
    val thing = apply(file)
    thing.filter(f => (vows.filter(vo => (f.contains(vo))).isEmpty))
  }
 
  def wordsMatchingRegexp(file: String, re: Regex): Iterator[String] = 
  {
    val thing = apply(file)
    thing.filter(x => !(re findAllMatchIn x).isEmpty)
  }

}

// vim: set ts=2 sw=2 et:


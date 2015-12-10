import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import java.io.PrintWriter
import Lines._

object Words {

  def apply(file: String) : Iterator[String] =  
    {
     val f = new java.io.File(file)
     try{
       scala.io.Source.fromFile(f).getLines().map(_.toLowerCase()).toIterator;
     }
     catch
     {
       case e: java.io.FileNotFoundException =>
         {
         println("No file exists")
         Iterator()
         
       }
         
     }
    }     
  
  def groupFreq[A,B](xs: Iterator[A], f: A => B): HashMap[B, Int] = {
      val iter = xs.map(f)
      iter.foldRight(HashMap[B, Int]())((n, countmap) => countmap + (n -> (countmap.getOrElse(n, 0)+1)))
  
  }
  
   

  val inti = List(1,21,5,39,12,7,92)

  def isEven(x: Int): String =
    if ((x % 2) == 0) { "Even" } else { "Odd" } 

  def sizeFreq(file: String): HashMap[Int, Int] = 
  {
    val sizeF = new HashMap[Int,Int]
    val myfile = apply(file)
    groupFreq[String,Int](myfile, (word => word.length))
  }     

  def charFreq(file: String): HashMap[Char, Int] = 
  {
    val chars   = scala.io.Source.fromFile(file).toIterator
    val grouper = (c:Char)=>c.toLower
    groupFreq(chars, grouper) 
  }

  def wordsOfSize(file: String, size: Int) : Iterator[String] = 
   apply(file).filter(_.length()==size)
   
  def wordsWithAllVowels(file: String): Iterator[String] = 
  for (x<-apply(file);
    if hasAllvowels(x))
    yield x

def hasAllvowels(x:String) : Boolean =
  x.contains('a')&&
      x.contains('e') &&
      x.contains('i') &&
      x.contains('o') &&
      x.contains('u')

      
      
  
 
  def wordsWithNoVowels(file: String): Iterator[String] = 
  for (x<-apply(file);
   if hasNovowels(x) == false)
    yield x

def hasNovowels(x:String) : Boolean =
  x.contains('a')||
      x.contains('e') ||
      x.contains('i') ||
      x.contains('o') ||
      x.contains('u')
 
  def wordsMatchingRegexp(file: String, re: Regex): Iterator[String] = 
    apply(file).filter(word => (re findAllIn(word)).size==1)


}

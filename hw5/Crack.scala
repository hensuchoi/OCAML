import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import Crypt._
import java.io.PrintWriter
import java.io.File
import java.io.Console
import java.io.BufferedWriter
//import Words._

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
    val field = line.split(":")
    Entry(field(0), field(1), Integer.valueOf(field(2)), Integer.valueOf(field(3)), field(4), field(5), field(6))
  }

}

object Crack {

  def transformReverse(w: String) : Iterator[String] = {
    List(w,w.reverse).toIterator
      
  }
  
  def transformCapitalize(w: String) : Iterator[String] = {
   if (w =="")
     {Iterator (w)}
   else 
    for (c<-Iterator (w.substring(0,1).toUpperCase, w.substring(0,1));
      s<- transformCapitalize(w.substring(1)))
    yield (c+s)
  }
      
  
  def transformDigits(w:String) : Iterator[String] = {
    if (w =="")
     {Iterator (w)}
   else 
   {
     val chars =     
     w.substring(0,1).toLowerCase match{
      case "o" => Iterator(w.substring(0,1),"0")
      case "z" =>  Iterator(w.substring(0,1),"2")
      case "a" =>  Iterator(w.substring(0,1),"4")
      case "b" =>  Iterator(w.substring(0,1),"6","8")
      case "g" | "q" =>  Iterator(w.substring(0,1),"9")
      case "i" | "l" =>  Iterator(w.substring(0,1),"1")
      case "e" => Iterator(w.substring(0,1),"3")
      case "s" =>  Iterator(w.substring(0,1),"5")
      case "t" => Iterator(w.substring(0,1),"7")
      case _ => Iterator(w.substring(0,1))
      }
     
     for(hd <- chars; tl <- transformDigits(w.substring(1)))
      yield(hd + tl)
      
      
   }
  }
     
     
   

  def checkPassword(plain: String, enc: String) : Boolean = 
    Crypt.crypt(enc, plain) == enc
 
  def candidateWords(file: String) =
    Words.wordsMatchingRegexp(file, new Regex("""^.{6,8}$"""))

  // scala> Crack("passwd", "words", "soln.1")
  // goto> scala Crack passwd words soln.1
    
  def getFile(file: String) : Iterator[String] =  
    {
     val f = new java.io.File(file)
     try{
       scala.io.Source.fromFile(f).getLines()
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
  


 def apply(pwdFile: String, wordsFile: String, outFile: String) : Unit = {

  //val cword = candidateWords(wordsFile).getLines().toIterator
  val login = scala.io.Source.fromFile(pwdFile).getLines().map(Entry.apply)
  val cwords =  getFile(wordsFile).toList.sortBy(_.length())
  val words =  scala.io.Source.fromFile(wordsFile).getLines().toIterator
  val output = new PrintWriter(new File(outFile))
  for(i<-login){
    if (apply1 (i,wordsFile,outFile,pwdFile) != "")
    {
       output.write(i.account + "=" + (apply1 (i,wordsFile,outFile,pwdFile) + "\n"))
       output.flush();
   }
      
   
   }
//output.close()
  }

  
  def apply1(login: Entry, wordsFile: String, outFile: String,pwdFile: String) : String = {  
  //val login = scala.io.Source.fromFile(pwdFile).getLines().map(Entry.apply)
  val words =  scala.io.Source.fromFile(wordsFile).getLines().toIterator
  //val output = new BufferedWriter(new File(outFile))
  val cwords = candidateWords(wordsFile)
  
  for(w<-cwords){
        //println(words.next())
        if(crackPW(w,login.password) == true){
                   
           
           //output.flush();
           return w;
        }
      }
  //output.close()
  return ""
  }


def crackPW(word:String, password:String):Boolean = {
    
    
    
    if (checkPassword(word,password)){
 println("false") 
       return true      
       } 
       else
       return       false

 val w1 = transformCapitalize(word)
    for(cap <- w1){
      if(checkPassword(cap,password)){
      	 println("false") 
        return true
      }
    }
    val w2= transformReverse(word) 
    for(rev <- w2){
      if(checkPassword(rev,password)){
      	 println("false") 
        return true
      }
    }
      val w3= transformDigits(word)
    for(dig <- w3){
      //println(dig)
      if(checkPassword(dig,password)){
      	 println("false") 
        return true
      }
    }
      
      
      
   
    return false
  }




  
  
  
  
  def main(args: Array[String]) = { 
    println("Begin: Cracking Passwords")
    apply(args(0), args(1), args(2));
    println("Done: Cracking Passwords")
  }
 }


// vim: set ts=2 sw=2 et:



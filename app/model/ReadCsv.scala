package model

import scala.io.Source
import javax.inject._
import scala.io.Codec.string2codec


trait ReadData
{
 def top10():List[(Int,String)]
 def searchData(code:String,country:String):Set[(String,String,String,String,String)]
 def typeOfRunways():List[(Int,String)]
  def runwayIndication():List[(Int,String)]
}

case class FormData(
      
      val code:String,
 val country:String,
 val typ:String
    )
 case class Airports(
      
      val id : String,
      val ident:String,
      val name :String,
      val iso_country:String, //same as code
      val iso_region:String      
    )
    case class Country (
 val code:String,
 val name:String

 )
    case class Runways (
val id: String,
//val name:String,
val airport_ref:String,//airport ID
val airport_ident:String,
val surface:String,
val le_ident:String

 )
 
 
@Singleton
class ReadCsv extends ReadData {
//object ReadCsv extends App {
 
  //def readfiles ={  
  val srcAP = Source.fromFile("C:\\Users\\ndabas\\Desktop\\resources\\airports.csv")("UTF-8").getLines().drop(0).map(_.replaceAll("\"","")).map(_.split(",",-1)).map(x=>Airports(x(0),x(1),x(3),x(8),x(9))).toList;
   val srcCT = Source.fromFile("C:\\Users\\ndabas\\Desktop\\resources\\countries.csv")("UTF-8").getLines().drop(0).map(_.replaceAll("\"","")).map(_.split(",",-1)).map(x=>Country(x(1),x(2))).toList;
    val srcRN = Source.fromFile("C:\\Users\\ndabas\\Desktop\\resources\\runways.csv")("UTF-8").getLines().drop(0).map(_.replaceAll("\"","")).map(_.split(",",-1)).map(x=>Runways(x(0),x(1),x(2),x(5),x(8))).toList;
 // }
    
    implicit class FilterHelper[A](l: Iterator[A]) {
  def ifthen(cond: Boolean, f1:(Iterator[A]) => Iterator[A],f2:(Iterator[A]) => Iterator[A]) = {
    if (cond) f1(l) else f2(l)
  }
} 
   
   
 
   
   def countryCodeLkp(country:String)  = {
   
   //srcCT.getLines().drop(0).map(_.replaceAll("\"","")).map(_.split(",",-1)).map(x=>Country(x(1),x(2))).filter(_.name==country).toList
   srcCT.filter(_.name.toUpperCase()==country.toUpperCase()).map(_.code).mkString
 }
    def countryLkp(code:String)  = {
   
   //srcCT.getLines().drop(0).map(_.replaceAll("\"","")).map(_.split(",",-1)).map(x=>Country(x(1),x(2))).filter(_.name==country).toList
   srcCT.filter(_.code==code).map(_.name).mkString
 }
 
 def airportLkp(countryCode:String,full:Boolean)={
   
   //srcAP.getLines().drop(0).map(_.replaceAll("\"","")).map(_.split(",",-1)).map(x=>Airports(x(0),x(1),x(3),x(8),x(9)))
   srcAP.filter(x=>full || (x.iso_country == countryCode) )
   .map(x=>(x.id->x)).toMap;
       
 }
 
 def runwaysLkp()={
   
   srcRN.map(runway=>(runway.airport_ref->runway)).toMap
 }
 
 def runwaysAirpotsjoin(airports:Map[String,Airports],runways:Map[String,Runways])=
 {
   //airports.keySet.foreach(print)
   airports.keySet.map(x=>(airports.getOrElse(x, null),runways.getOrElse(x,null ))).filter(_._2!=null).map(x=>(x._1.iso_country,x._1.name,x._2.airport_ident,x._2.surface,x._2.le_ident))
   
 }
  //val iterAP = srcAP.getLines().map(_.split(",")).map(x=>Airports(x(0).replaceAll("\"",""),x(1).replaceAll("\"",""),x(3).replaceAll("\"",""),x(8).replaceAll("\"",""),x(9).replaceAll("\"","")))
 // .ifthen(search=="code", _.filter(_.iso_country == code),_.filter(_.name == country));
  //.filter(_.iso_country == country);
    
 //countryCodeLkp("United Arab Emirates"). foreach(x=>print(x))
 
   //airportLkp("AE",false).keys. foreach(x=>println(x))
 override def searchData(code:String,country:String) =
 {
   var search:String=null
   //search=search+"cnt:"+country+"cd:"+code
     if(country != null && country != "")
     {
       
      search=countryCodeLkp(country)
      //search=search+":country"
     }
   else {
     
     search=code
     //search=search+":code"
   }
   //Set{(search,search,search,search,search)}
   runwaysAirpotsjoin(airportLkp(search,false),runwaysLkp())
 
 }
 //val x=runwaysAirpotsjoin(airportLkp("AE",true),runwaysLkp())
 //x.foreach(println)
 
 //top and bottom list
 override def top10():List[(Int,String)] =
 {
   val x=runwaysAirpotsjoin(airportLkp("",true),runwaysLkp());
    //print(msg)
 val y=x.toSeq.groupBy(x=>x._1).map(t => (t._2.length,t._1)).toList.sorted;
  (y.take(10)++y.takeRight(10)).map{case(x,y)=>(x,countryLkp(y))}
 
 }

override def typeOfRunways() ={
val x=runwaysAirpotsjoin(airportLkp("",true),runwaysLkp());
x.groupBy(x=>(x._1,x._4)).keys.groupBy(_._1).map{case(k,v)=>(v.map(_._2).toList.length,k)}.toList
}

override def runwayIndication () ={
// runway indication
  val x=runwaysAirpotsjoin(airportLkp("",true),runwaysLkp());
   x.toSeq.groupBy(x=>(x._5)).map(t => (t._2.length,t._1)).toList.sorted.takeRight(10)
  
}

// the rest of iter is not processed
//srcAP.close()
  
  
}
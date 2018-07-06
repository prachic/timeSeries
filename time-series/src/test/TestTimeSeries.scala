package test

import scala.collection.immutable.SortedMap
import scala.io.Source
import scala.math.rint
import java.io.FileNotFoundException

/***
* @author: PrachiC
* Time-Series challenge - Reads file from local file path passed as argument
* Reads a file with series of timeStamp and price ratio
* Prints max value, min value, rolling_sum, occurrences for price ratio for the sliding window of 'T' sec
* Date: 05 July 2018
*/
object TestTimeSeries { 

    def main(args: Array[String]): Unit = {
		if(args == null || args.length == 0) {
			throw new Exception("Please enter the filePath for execution")
		}
		run(args(0)) 
	}

  /***
   * Reads the file at specified local file path and prints the time-series
   */
	def run(path: String): Boolean = {
	  var res : Boolean = false
      if(path == null || path.length == 0) {
        throw new Exception("Please enter the filePath")
      }      
	  try {
		  val lines = Source.fromFile(path).getLines().toList     
      
		  // Get a list of tuples in sorted order
		  val dataList = lines.map(parseLine).toList.toSeq.sortBy(_._1) 
		  res = printTimeSeries(dataList)
	    } catch {
		    case e: FileNotFoundException => {
			    println(s"File not found at path : $path")
			    //e.printStackTrace
		    }
		    case exp: Exception => {
			    println(s"Exception occurred while executing time-series for path : $path")
			    //exp.printStackTrace
		    }
	    }
	    res
    } 
  
    /**
     * Prints time series for given input data
     */
    def printTimeSeries(dataList: Seq[(Long,Double)]) : Boolean = {
      // Initialing variables
      var max: Double = 0.0
      var min: Double = 0.0
      var rollingSum : Double = 0.0
      var num: Int = 0
      var value: Double = 0.0
      var windowTime : Long = 0
      var time: Long = 0
      println("Time \t\tValue \t\tN_O \tRoll_Sum \tMin_Value \tMax_Value")
      println("-----------------------------------------------------------------")
      dataList.foreach(tuple => {
        time = tuple._1
        value = tuple._2   
        // Checking rolling window time
        if((time - windowTime) < Constants.WindowTimeInSec) {
          num += 1
        	rollingSum += value
          if(value > max) { max = value }        
          if(value < min) { min = value }        
        } else {
          windowTime = time
          max = value
          min = value
          rollingSum = value
          num = 1
        }
        // Rounding up to 5 decimal places
        rollingSum = (math rint rollingSum * 100000)/100000
        println(s"$time \t$value \t$num \t$rollingSum \t$min \t$max")
      })      
      println("-----------------------------------------------------------------")
	  true
  }
    
    /**
     * Parse each line and return a tuple of timeStamp and priceRatio
     */
    def parseLine(line: String) = {
      val fields = line.split(Constants.delimiter)
      (fields(Constants.TimePosition).toLong, fields(Constants.ValuePosition).toDouble)
    }    
}

/***
 * Constants for time-series
 */
object Constants {
  val WindowTimeInSec = 60
  val TimePosition = 0
  val ValuePosition = 1
  val delimiter = " " 
}
package recfun
import common._
import scala.util.control.Breaks

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =  BigInt(r).pow(c).intValue

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    var status =false;
     var tempBraCount =0;
    
    val loop = new Breaks;
      loop.breakable {
    for(c <- chars){
      if(c == '(' )
       tempBraCount += 1;
      
      if( c== ')'){
    	 	
    	  tempBraCount -= 1;
    	  if(tempBraCount == -1){
    	   false
    	  }
      }
      
    }
      }
    if(tempBraCount != 0){
      status =false;
    }else{
      status = true;
    }
    
    status;
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {


    countCoins(money, coins.sortWith(_.compareTo(_) < 0))
  }
  
  def countCoins(tempMoney: Int, tempCoins: List[Int]): Int = {
       if(tempMoney == 0) 
          1
       else if(tempMoney < 0) 
          0
       else if(tempCoins.isEmpty && tempMoney>=1 )
          0
       else
          countCoins(tempMoney, tempCoins.tail) + countCoins(tempMoney - tempCoins.head, tempCoins)
   }
  

}

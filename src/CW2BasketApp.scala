import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object CW2BasketApp extends App{

  //STORE DATA READ FROM FILE
  val foodData = readFile("data.txt")
  //println(foodData)
  //READ FILE
  def readFile(filename:String):Map[String,List[Int]]={
    var mapBuffer: Map[String, List[Int]] = Map()
    try{
      for(line <- Source.fromFile(filename).getLines()){
        val splitline = line.split(",").map(_.trim).toList

        mapBuffer = mapBuffer ++ Map(splitline.head -> splitline.tail.map(t => t.toInt))
      }
    }catch{
      case ex:Exception => println("Sorry, an exception happened" + ex)
    }
    mapBuffer
  }
  //MAP MENU OPTION CHOICE TO FUNCTION
  val menuMap = Map[Int, () => Boolean] (
    1 -> menu1,
    2 -> menu2,
    3 -> menu3,
    4 -> menu4,
    5 -> menu5,
    6 -> menu6,
    7 -> menu7)
  // MENU CONTROL/DISPLAY
  var opt = 0
  do{
    opt = readOption
  } while(menu(opt))

  def readOption: Int = {
        println(
          """|Please select one of the following:
             |  1 - Current Price for each food
             |  2 - Get highest price for each food
             |  3 - Get Lowest price for each food
             |  4 - Get Median price for each food
             |  5 - Compare average prices
             |  6 - Create a Shopping Basket
             |  7 - quit""".stripMargin)
  readInt()
  }
  // MAPPING USER INPUT TO MENU MAP FUNCTIONS
  def menu(option: Int): Boolean = {
    menuMap.get(option) match {
      case Some(f) => f()
      case None => println("Sorry, Menu Choice not recognized, please select 1-7")
      true
    }
  }
  //HANDLERS FOR MENU OPTION CHOICE
  def menu1(): Boolean = {
    println("---Getting Current Food Prices---")
    val currentFoodPrice = getCurrentFoodPrice(foodData)
    currentFoodPrice foreach  {case (x, y) => println(f"$x: £${y.toDouble/100}%.2f")}
    println("---End of Current Food Prices---\n")
    true
  }
  def menu2(): Boolean = {
    println("---Getting Highest Food Price---")
    val highestFoodPrices = maxFoodPrice(foodData)
    highestFoodPrices foreach {case (x, y) => println(f"$x: £${y.toDouble/100}%.2f")}
    println("---End of Highest Food Price---\n")
    true
  }
  def menu3(): Boolean = {
    println("---Getting Lowest Food Price---")
    val lowestFoodPrices = minFoodPrice(foodData)
    lowestFoodPrices foreach {case (x, y) => println(f"$x: £${y.toDouble/100}%.2f")}
    println("---End of Lowest Food Price---\n")
    true
  }
  def menu4(): Boolean = {
    println("---Getting Median Food Price")
    val medianFoodPrices = medianFoodPrice(foodData)
    medianFoodPrices foreach{case(x, y) => println(f"$x: £${y.toDouble/100}%.2f")}
    println("---End of Median Food Price")
    true
  }
  def menu5(): Boolean = {
    println("---Price Comparison---")
    println("Please enter 1st food to compare")
    val food1 = readLine().toUpperCase
    if(food1.isBlank) {
      println("1st food can't be blank")
      return true
    }

    println(s"please enter 2nd food to compare with $food1")
    val food2 = readLine().toUpperCase
    if(food2.isBlank) {
      println("2nd food can't be blank")
      return true
    }

    val medianFoodPrices = medianFoodPrice(foodData)
    println(s"Price Comparison of $food1 vs $food2")
    compareFood(medianFoodPrices, food1, food2) match{
      case  Success((price1, price2, diff)) =>
        println(
          f"""
             |$food1 average cost: £$price1%.2f
             |$food2 average cost: £$price2%.2f
             |The difference between them is: £$diff%.2f
             |""".stripMargin)
      case Failure(ex) =>
        println(s"Error: ${ex.getMessage}")
    }
    println("---End of Price Comparison---\n")
    true
  }
  def menu6(): Boolean = {
    val foodList = getItems()

    calculateBasketCost(foodList) match{
      case Success((foodCost, basketTotalCost)) => println("---Shopping Basket---")
      foodCost.foreach{
        case (food, amount, singleCost, foodCostPrice) => println(f"$food - Single Price: £$singleCost%.2f - quantity: $amount%.2f - Food Cost Price £$foodCostPrice%.2f")
      }
      println(f"total basket cost : £$basketTotalCost%.2f")
      println("---End of Basket---\n")

      case Failure(ex) => println(s"Error calculating shopping basket: ${ex.getMessage}")
    }
    true
  }
  def menu7():Boolean ={
    println("You selected 7 - Quitting Application...")
    false
  }
  // MENU FUNCTIONS
  def getCurrentFoodPrice(foodData: Map[String, List[Int]]):Map[String,Int] =
    foodData.map{case (f, p) => (f, p.last)}
  def maxFoodPrice(foodData: Map[String, List[Int]]):Map[String, Int] =
    foodData.map{case (f, p) => (f, p.max)}
  def minFoodPrice(foodData: Map[String, List[Int]]):Map[String, Int] =
    foodData.map{case(f, p) => (f, p.min)}
  def medianFoodPrice(foodData: Map[String, List[Int]]): Map[String, Int] =
    foodData.map{case (f, p) => (f, p.sum/p.length)}
  def compareFood(medianPrice: Map[String, Int], food1:String, food2:String):Try[(Double, Double, Double)] = {
    (getFoodItem(medianPrice, food1), getFoodItem(medianPrice, food2)) match{
      case (Some((_, price1)), Some((_,price2))) =>
        if(price1 > price2)
          Success(price1, price2, (price1 - price2))
        else
          Success(price1, price2, (price2 - price1))
      case (None, _) => Failure(new Exception(s"${food1} not found"))
      case (_, None) => Failure(new Exception(s"${food2} not found"))

    }
  }
  def calculateBasketCost(foodList: List[(String, Float)]):Try[(List[(String, Float, Float, Float)],Double)] = Try {
    val currentPrices = getCurrentFoodPrice(foodData)

    val foodCost = foodList.map {
      case (food, amount) =>
        currentPrices.get(food) match{
          case Some(cost) =>
          val singlePrice = cost.toFloat / 100
          val foodCostPrice = singlePrice * amount
            (food, amount, singlePrice, foodCostPrice)
          case None => throw new Exception(s"$food not found in current price list")
        }
    }
    val basketCostTotal = foodCost.map(_._4).sum
    if(basketCostTotal == 0){
      throw new Exception("Basket is empty")
    }else{
      (foodCost, basketCostTotal)
    }
      }
  // HELPER FUNCTIONS
  def getItems(): List[(String, Float)] = {
    @tailrec
    def makeShoppingList(basketList: List[(String, Float)]): List[(String, Float)] = {
      println("Enter food name or 'done' to finish:")
      val foodName = readLine().toUpperCase

      if(foodName.isBlank) {
        println("Invalid food - please enter a food item")
         makeShoppingList(basketList)
      }else{
        if (foodName == "DONE")
          basketList.reverse
        else {
          Try {
            println("Enter quantity:")
            val quantity = readLine().toFloat
            (foodName, quantity)
          } match {
            case Success(item) => makeShoppingList(item :: basketList)
            case Failure(_) =>
              println("Invalid quantity - please enter a number")
              makeShoppingList(basketList)
        }
        }
      }
    }
    makeShoppingList(List())
  }
  def getFoodItem(medianPrice: Map[String, Int], foodItem:String): Option[(String,Double)] = {
    medianPrice.get(foodItem).map(p => (foodItem, p.toDouble/100))
  }
}

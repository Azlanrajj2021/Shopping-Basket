package shoppingbasket

import scala.io.StdIn
import scala.math.BigDecimal.{RoundingMode, double2bigDecimal}

object ShoppingBasket extends App {
  println(
    """Welcome to the online grocery store. We have:
      |1: Soup - £0.65 per tin
      |2: Bread - £0.80 per loaf
      |3: Milk - £1.30 per bottle
      |4: Apples - £1.00 per bag
      |
      |Offers available are:
      |10% off on Apples
      |Buy 2 soup tins and get 1 bread loaf half-price""".stripMargin)
  //Items map represents the prices for the different grocery items.
  val items = Map(1 -> 0.65, 2 -> 0.80, 3 -> 1.30, 4 -> 1.00)
  try {
    //userInputString uses the StdIn library to read user input from the console. It allows the program to prompt the user for input and wait until the user inputs text.
    val userInputString = StdIn.readLine(
      """Please input the number for the item you would like.
        |If you would like multiple items input the item numbers all together (please dont put any spaces or commas between the item numbers).
        |See the below example for multiple items.
        |Example: I want one soup tin, one milk and two bread loafs
        |I would type: 1322
        |Now please choose your items:
        |""".stripMargin)
    //Checks if user input only consists of digits.
    if (userInputString.forall(Character.isDigit)) {
      //Counts the number of Apples, Tins and Breads in the userInputString to be later used to check for offers.
      val numberOfSoupTins = userInputString.count(_ == '1')
      val numberOfBreadLoafs = userInputString.count(_ == '2')
      val numberOfAppleBags = userInputString.count(_ == '4')
      //In val subTotal the first map makes the userInputString a digit.
      //Second map takes each digit and puts it through the val items which maps it to it's price.
      //As the digits are mapped to it's price they are being summed up, so once complete it will give the subTotal price for all the items selected by the user.
      val subTotal = userInputString.map(_.asDigit).map(d => items(d)).sum
      //Prints the subtotal price which is rounded using the setScale method and setting it to 2 decimal places (since it's currency).
      //RoundingMode Half_up used so that it rounds the price up if the third decimal is >= 5.
      println("Subtotal is: £" + subTotal.setScale(2, RoundingMode.HALF_UP) + ".")
      //val offers and val discounts of type string and bigDecimal respectively, are assigned based on the match expression.
      //val offers is a string message to say which discounts you are getting.
      //val discounts works out the numerical value of the discount to be applied to the subTotal (given as a bigDecimal for precision and accuracy).
      val (offers, discounts): (String, BigDecimal) = (numberOfSoupTins, numberOfBreadLoafs, numberOfAppleBags) match {
        //Discount case for when the basket has apple bags, bread loafs, and if the amount of tins in the basket are equal to or more than 2.
        case (tins, loafs, bags) if tins >= 2 && loafs >= 1 && bags >= 1 =>
          //breadDiscount checks on how many bread loafs the half-price discount applies.
          //If one bread loaf is half-price for every two tins, then number of tins is divided by two and the resulting number will give use the amount of bread loafs eligible for discount.
          //If tins = 3 and loafs = 6 then tins/2 = 3/2 = 1 (rounded down to nearest number).
          //math.min(1, 6) --> math.min takes the lower of the two values. In this case one.
          //0.5 is the 50% discount. items(2) --> 2 is the number for bread loaf and it is mapped in items to get bread loaf price which is £0.80.
          //So in this case val breadDiscount = 1 * (0.5 * 0.80) = £0.40.
          val breadDiscount = math.min(tins / 2, numberOfBreadLoafs) * (0.5 * items(2))
          ("Offers include: Apples 10% Off and Bread half-price.", numberOfAppleBags * (items(4) * 0.1) + breadDiscount)
        //Discount case for when the basket has no apples, bread loafs and if the amount of tins in the basket are equal to or more than 2.
        case (tins, loafs, bags) if tins >= 2 && loafs >= 1 =>
          val breadDiscount = math.min(tins / 2, numberOfBreadLoafs) * (0.5 * items(2))
          ("Offers include: Bread half-price.", breadDiscount)
        //Discount case for when the basket has just apples.
        case (tins, loafs, bags) if bags >= 1 => ("Offers include: Apples 10% Off.", numberOfAppleBags * (items(4) * 0.1))
        //For any other case's the below is printed since no offers and discounts are applicable.
        case _ => ("Sorry, no offers available.", 0)
      }
      //Converts subTotalPrice to BigDecimal so that we can subtract the discount to give the TotalPrice.
      //After the subtraction, it is rounded up to 2 decimal places.
      val totalPrice = (BigDecimal(subTotal) - discounts).setScale(2, RoundingMode.HALF_UP)
      //Prints the offer statement based on the match expression.
      println(offers)
      //Prints the totalPrice of the basket after the discount (if any).
      println(s"The total price is: £$totalPrice.")
    }
    //If the userInputString includes anything other than numbers, it will trigger the else statement.
    else {
      //Prints the statement below and terminates the program.
      println("Invalid inputs. Please try again.")
    }
    //If there are any runtime errors, the below general exception case e will catch it and print the error message.
  } catch {
    case e: Exception => "Error: " + e.getMessage
  }
}

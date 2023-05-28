package shoppingbasket
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper
import shoppingbasket.ShoppingBasket.{items, _}

import java.io.ByteArrayInputStream
import scala.math.BigDecimal.{RoundingMode, double2bigDecimal}


class ShoppingBasketSpec extends FunSuite {

  test("subTotal should take userInputString calculate the sum of price") {
    val expected = 2.75
    val userInputString = "123"

    val items = Map(1 -> 0.65, 2 -> 0.80, 3 -> 1.30, 4 -> 1.00)
    val subTotal = userInputString.map(_.asDigit).map(d => items(d)).sum

    assert (subTotal == expected)
  }
  test("numberOfTins should count all the 1s in userInputString") {
    val expected = 3
    val userInputString = "2,3,1,2,1,1"
    val numberOfSoupTins = userInputString.count(_ == '1')

    assert(numberOfSoupTins == expected)
  }
  test("correctly calculate the offers and discounts for when the basket has tins, loafs and bags") {
    val numberOfSoupTins = 4
    val numberOfBreadLoafs = 10
    val numberOfAppleBags = 4
    val items = Map(1 -> 0.65, 2 -> 0.80, 3 -> 1.30, 4 -> 1.00)

    val (offers, discounts): (String, BigDecimal) = (numberOfSoupTins, numberOfBreadLoafs, numberOfAppleBags) match {
      case (tins, loafs, bags) if tins >= 2 && loafs >= 1 && bags >= 1 =>
        val breadDiscount = math.min(tins / 2, numberOfBreadLoafs) * (0.5 * items(2))
        ("Offers include: Apples 10% Off and Bread half-price.", numberOfAppleBags * (items(4) * 0.1) + breadDiscount)
      case (tins, loafs, bags) if tins >= 2 && loafs >= 1 =>
        val breadDiscount = math.min(tins / 2, numberOfBreadLoafs) * (0.5 * items(2))
        ("Offers include: Bread half-price.", breadDiscount)
      case (tins, loafs, bags) if bags >= 1 => ("Offers include: Apples 10% Off.", numberOfAppleBags * (items(4) * 0.1))
      case _ => ("Sorry, no offers available.", 0)
    }
    val roundedDiscounts = discounts.setScale(2, BigDecimal.RoundingMode.HALF_UP)

    assert(offers == "Offers include: Apples 10% Off and Bread half-price.")
    assert(roundedDiscounts == 1.20)
  }
  test("correctly calculate the offers and discounts for when the basket has tins and loafs (number of loafs higher compared to tins") {
    val numberOfSoupTins = 4
    val numberOfBreadLoafs = 10
    val numberOfAppleBags = 0
    val items = Map(1 -> 0.65, 2 -> 0.80, 3 -> 1.30, 4 -> 1.00)

    val (offers, discounts): (String, BigDecimal) = (numberOfSoupTins, numberOfBreadLoafs, numberOfAppleBags) match {
      case (tins, loafs, bags) if tins >= 2 && loafs >= 1 && bags >= 1 =>
        val breadDiscount = math.min(tins / 2, numberOfBreadLoafs) * (0.5 * items(2))
        ("Offers include: Apples 10% Off and Bread half-price.", numberOfAppleBags * (items(4) * 0.1) + breadDiscount)
      case (tins, loafs, bags) if tins >= 2 && loafs >= 1 =>
        val breadDiscount = math.min(tins / 2, numberOfBreadLoafs) * (0.5 * items(2))
        ("Offers include: Bread half-price.", breadDiscount)
      case (tins, loafs, bags) if bags >= 1 => ("Offers include: Apples 10% Off.", numberOfAppleBags * (items(4) * 0.1))
      case _ => ("Sorry, no offers available.", 0)
    }
    val roundedDiscounts = discounts.setScale(2, BigDecimal.RoundingMode.HALF_UP)

    assert(offers == "Offers include: Bread half-price.")
    assert(roundedDiscounts == 0.80)
  }

  test("correctly calculate the offers and discounts for when the basket has tins and loafs (number of tins higher compared to loafs") {
    val numberOfSoupTins = 7
    val numberOfBreadLoafs = 3
    val numberOfAppleBags = 0
    val items = Map(1 -> 0.65, 2 -> 0.80, 3 -> 1.30, 4 -> 1.00)

    val (offers, discounts): (String, BigDecimal) = (numberOfSoupTins, numberOfBreadLoafs, numberOfAppleBags) match {
      case (tins, loafs, bags) if tins >= 2 && loafs >= 1 && bags >= 1 =>
        val breadDiscount = math.min(tins / 2, numberOfBreadLoafs) * (0.5 * items(2))
        ("Offers include: Apples 10% Off and Bread half-price.", numberOfAppleBags * (items(4) * 0.1) + breadDiscount)
      case (tins, loafs, bags) if tins >= 2 && loafs >= 1 =>
        val breadDiscount = math.min(tins / 2, numberOfBreadLoafs) * (0.5 * items(2))
        ("Offers include: Bread half-price.", breadDiscount)
      case (tins, loafs, bags) if bags >= 1 => ("Offers include: Apples 10% Off.", numberOfAppleBags * (items(4) * 0.1))
      case _ => ("Sorry, no offers available.", 0)
    }
    val roundedDiscounts = discounts.setScale(2, BigDecimal.RoundingMode.HALF_UP)

    assert(offers == "Offers include: Bread half-price.")
    assert(roundedDiscounts == 1.20)
  }

  test("correctly calculate the offers and discounts for when the basket has only apples") {
    val numberOfSoupTins = 0
    val numberOfBreadLoafs = 0
    val numberOfAppleBags = 7
    val items = Map(1 -> 0.65, 2 -> 0.80, 3 -> 1.30, 4 -> 1.00)

    val (offers, discounts): (String, BigDecimal) = (numberOfSoupTins, numberOfBreadLoafs, numberOfAppleBags) match {
      case (tins, loafs, bags) if tins >= 2 && loafs >= 1 && bags >= 1 =>
        val breadDiscount = math.min(tins / 2, numberOfBreadLoafs) * (0.5 * items(2))
        ("Offers include: Apples 10% Off and Bread half-price.", numberOfAppleBags * (items(4) * 0.1) + breadDiscount)
      case (tins, loafs, bags) if tins >= 2 && loafs >= 1 =>
        val breadDiscount = math.min(tins / 2, numberOfBreadLoafs) * (0.5 * items(2))
        ("Offers include: Bread half-price.", breadDiscount)
      case (tins, loafs, bags) if bags >= 1 => ("Offers include: Apples 10% Off.", numberOfAppleBags * (items(4) * 0.1))
      case _ => ("Sorry, no offers available.", 0)
    }
    val roundedDiscounts = discounts.setScale(2, BigDecimal.RoundingMode.HALF_UP)

    assert(offers == "Offers include: Apples 10% Off.")
    assert(roundedDiscounts == 0.70)
  }

  test("correctly calculate the offers and discounts for when no offers apply") {
    val numberOfSoupTins = 1
    val numberOfBreadLoafs = 6
    val numberOfAppleBags = 0
    val items = Map(1 -> 0.65, 2 -> 0.80, 3 -> 1.30, 4 -> 1.00)

    val (offers, discounts): (String, BigDecimal) = (numberOfSoupTins, numberOfBreadLoafs, numberOfAppleBags) match {
      case (tins, loafs, bags) if tins >= 2 && loafs >= 1 && bags >= 1 =>
        val breadDiscount = math.min(tins / 2, numberOfBreadLoafs) * (0.5 * items(2))
        ("Offers include: Apples 10% Off and Bread half-price.", numberOfAppleBags * (items(4) * 0.1) + breadDiscount)
      case (tins, loafs, bags) if tins >= 2 && loafs >= 1 =>
        val breadDiscount = math.min(tins / 2, numberOfBreadLoafs) * (0.5 * items(2))
        ("Offers include: Bread half-price.", breadDiscount)
      case (tins, loafs, bags) if bags >= 1 => ("Offers include: Apples 10% Off.", numberOfAppleBags * (items(4) * 0.1))
      case _ => ("Sorry, no offers available.", 0)
    }
    val roundedDiscounts = discounts.setScale(2, BigDecimal.RoundingMode.HALF_UP)

    assert(offers == "Sorry, no offers available.")
    assert(roundedDiscounts == 0)
  }

  test("should calculate the totalPrice from subTotal - discounts"){
    val subTotal = 7.55
    val discounts = 0.80
    val expected = 6.75

    val totalPrice = (BigDecimal(subTotal) - discounts).setScale(2, RoundingMode.HALF_UP)

    assert(totalPrice == expected)
  }
}


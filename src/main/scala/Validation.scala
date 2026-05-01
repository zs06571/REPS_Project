package com.reps

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.format.ResolverStyle
import scala.util.Try

object Validation {

  def isValidDate(date: String): Boolean = {
    val formatter = DateTimeFormatter
      .ofPattern("dd/MM/uuuu")
      .withResolverStyle(ResolverStyle.STRICT)

    Try {
      val parsedDate = LocalDate.parse(date, formatter)
      val year = parsedDate.getYear

      date.matches("""\d{2}/\d{2}/\d{4}""") &&
        year >= 1900 &&
        year <= 2100
    }.getOrElse(false)
  }

  def isValidTime(time: String): Boolean = {
    if (!time.matches("""\d{2}:\d{2}""")) {
      false
    } else {
      val parts = time.split(":")
      val hour = parts(0).toInt
      val minute = parts(1).toInt

      hour >= 0 && hour <= 23 &&
        minute >= 0 && minute <= 59
    }
  }

  def isValidHour(hour: String): Boolean = {
    hour.matches("""\d{2}""") && hour.toInt >= 0 && hour.toInt <= 23
  }

  def isValidMonth(month: String): Boolean = {
    month.matches("""\d{2}""") && month.toInt >= 1 && month.toInt <= 12
  }

  def isValidYear(year: String): Boolean = {
    year.matches("""\d{4}""") && year.toInt >= 1900 && year.toInt <= 2100
  }

  def validateGeneration(value: String): Either[String, Double] = {
    try {
      val number = value.toDouble
      if (number < 0) Left("Invalid generation value. Please enter a non-negative number.")
      else Right(number)
    } catch {
      case _: NumberFormatException =>
        Left("Invalid generation value. Please enter a non-negative number.")
    }
  }
}
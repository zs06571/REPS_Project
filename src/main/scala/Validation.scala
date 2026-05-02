// Shuo Zhao 002513249
// Hongyao Liu 002513919 
// Hongrui Zhang 002520436

package com.reps

// External references used in this file:
// Oracle. (n.d.). DateTimeFormatter (Java SE 17 & JDK 17). https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/time/format/DateTimeFormatter.html
// Oracle. (n.d.). ResolverStyle (Java Platform SE 8). https://docs.oracle.com/javase/8/docs/api/java/time/format/ResolverStyle.html
// DateTimeFormatter is used for parsing dates.
// ResolverStyle.STRICT is used for strict date validation.

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.format.ResolverStyle
import scala.util.Try

object Validation {

  // Check if the date is valid and in dd/MM/yyyy format
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

  // Check if the time is valid and in HH:MM format
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

  // Check if the hour is valid (0 - 23)
  def isValidHour(hour: String): Boolean = {
    hour.matches("""\d{2}""") && hour.toInt >= 0 && hour.toInt <= 23
  }

  // Check if the month is valid (1 - 12)
  def isValidMonth(month: String): Boolean = {
    month.matches("""\d{2}""") && month.toInt >= 1 && month.toInt <= 12
  }

  // Check if the year is valid (1900 - now)
  def isValidYear(year: String): Boolean = {
    val currentYear = java.time.LocalDate.now().getYear
    year.matches("""\d{4}""") && year.toInt >= 1900 && year.toInt <= currentYear
  }

  // Check if the generation value is a non-negative number
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

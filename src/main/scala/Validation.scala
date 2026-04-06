package com.reps

object Validation {

  def isValidDate(date: String): Boolean = {
    if (!date.matches("""\d{2}/\d{2}/\d{4}""")) {
      false
    } else {
      val parts = date.split("/")
      val day = parts(0).toInt
      val month = parts(1).toInt
      val year = parts(2).toInt

      month >= 1 && month <= 12 &&
        day >= 1 && day <= 31 &&
        year >= 1900 && year <= 2100
    }
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

  def validateGeneration(value: String): Either[String, Double] = {
    try {
      val number = value.toDouble
      if (number < 0) Left("Generation cannot be negative.")
      else Right(number)
    } catch {
      case _: NumberFormatException => Left("Invalid generation value.")
    }
  }

  def validateStorage(value: String): Either[String, Double] = {
    try {
      val number = value.toDouble
      if (number < 0) Left("Storage cannot be negative.")
      else Right(number)
    } catch {
      case _: NumberFormatException => Left("Invalid storage value.")
    }
  }

  def validateHealth(value: String): Either[String, Double] = {
    try {
      val number = value.toDouble
      if (number < 0 || number > 100) Left("Equipment health must be between 0 and 100.")
      else Right(number)
    } catch {
      case _: NumberFormatException => Left("Invalid equipment health value.")
    }
  }
}
package com.reps

import scala.io.StdIn.readLine

object MainApp {

  val filePath = "data/energy_data.csv"

  def showMenu(): Unit = {
    println("\n--- REPS Menu ---")
    println("1. View all records")
    println("2. Search by date")
    println("3. Filter by energy type")
    println("4. Filter by hour")
    println("5. Filter by month")
    println("6. Filter by date range")
    println("7. Sort by generation")
    println("8. Analyze generation data")
    println("9. Generate alerts")
    println("10. View control recommendations")
    println("11. Add new record")
    println("12. Exit")
  }

  def menuLoop(): Unit = {
    val records = FileIO.loadValidRecords(filePath)

    showMenu()
    val choice = readLine("Choose an option: ")

    choice match {
      case "1" =>
        println("\nAll records:")
        Utils.printRecords(records)
        menuLoop()

      case "2" =>
        val date = readLine("Enter date (DD/MM/YYYY): ")
        println(s"\nRecords for $date:")
        Utils.printRecords(QueryService.filterByDate(records, date))
        menuLoop()

      case "3" =>
        val input = readLine("Enter energy type (Solar/Wind/Hydro): ")

        val result = input match {
          case "Solar" => QueryService.filterByEnergyType(records, Solar)
          case "Wind"  => QueryService.filterByEnergyType(records, Wind)
          case "Hydro" => QueryService.filterByEnergyType(records, Hydro)
          case _ =>
            println("Invalid energy type.")
            Nil
        }

        if (result.nonEmpty) {
          println(s"\nFiltered records for $input:")
          Utils.printRecords(result)
        }

        menuLoop()

      case "4" =>
        val hour = readLine("Enter hour (HH): ")
        println(s"\nRecords for hour $hour:")
        Utils.printRecords(QueryService.filterByHour(records, hour))
        menuLoop()

      case "5" =>
        val month = readLine("Enter month (MM): ")
        val year = readLine("Enter year (YYYY): ")
        println(s"\nRecords for month $month/$year:")
        Utils.printRecords(QueryService.filterByMonth(records, month, year))
        menuLoop()

      case "6" =>
        val startDate = readLine("Enter start date (DD/MM/YYYY): ")
        val endDate = readLine("Enter end date (DD/MM/YYYY): ")
        println(s"\nRecords from $startDate to $endDate:")
        Utils.printRecords(QueryService.filterByDateRange(records, startDate, endDate))
        menuLoop()

      case "7" =>
        println("\nRecords sorted by generation ascending:")
        Utils.printRecords(QueryService.sortByGenerationAsc(records))
        menuLoop()

      case "8" =>
        println("\nGeneration analysis:")
        Utils.printAnalysis(AnalyticsService.analyzeGeneration(records))
        menuLoop()

      case "9" =>
        println("\nGenerated alerts:")
        val alerts = AlertService.generateAllAlerts(records)
        Utils.printAlerts(alerts)
        menuLoop()

      case "10" =>
        println("\nControl recommendations:")
        val alerts = AlertService.generateAllAlerts(records)
        val recommendations = ControlService.generateRecommendations(alerts)
        Utils.printRecommendations(recommendations)
        menuLoop()

      case "11" =>
        val record = createRecordFromInput()
        FileIO.appendRecord(filePath, record)
        println("New record added successfully.")
        menuLoop()

      case "12" =>
        println("Exiting REPS...")

      case _ =>
        println("Invalid menu option.")
        menuLoop()
    }
  }

  def readValidDate(): String = {
    val input = readLine("Enter date (DD/MM/YYYY): ")
    if (Validation.isValidDate(input)) {
      input
    } else {
      println("Invalid date format. Please use DD/MM/YYYY.")
      readValidDate()
    }
  }

  def readValidTime(): String = {
    val input = readLine("Enter time (HH:MM): ")
    if (Validation.isValidTime(input)) {
      input
    } else {
      println("Invalid time format. Please use HH:MM.")
      readValidTime()
    }
  }

  def readValidEnergyType(): EnergyType = {
    val input = readLine("Enter energy type (Solar/Wind/Hydro): ")
    Parser.parseEnergyType(input) match {
      case Right(value) => value
      case Left(_) =>
        println("Invalid energy type. Please enter Solar, Wind, or Hydro.")
        readValidEnergyType()
    }
  }

  def readValidGeneration(): Double = {
    val input = readLine("Enter generation: ")
    Validation.validateGeneration(input) match {
      case Right(value) => value
      case Left(error) =>
        println(error)
        readValidGeneration()
    }
  }

  def readValidStorage(): Double = {
    val input = readLine("Enter storage: ")
    Validation.validateStorage(input) match {
      case Right(value) => value
      case Left(error) =>
        println(error)
        readValidStorage()
    }
  }

  def readValidHealth(): Double = {
    val input = readLine("Enter equipment health (0-100): ")
    Validation.validateHealth(input) match {
      case Right(value) => value
      case Left(error) =>
        println(error)
        readValidHealth()
    }
  }

  def readValidStatus(): PlantStatus = {
    val input = readLine("Enter status (Normal/LowOutput/Malfunction/MaintenanceNeeded): ")
    Parser.parsePlantStatus(input) match {
      case Right(value) => value
      case Left(_) =>
        println("Invalid status. Please enter Normal, LowOutput, Malfunction, or MaintenanceNeeded.")
        readValidStatus()
    }
  }

  def createRecordFromInput(): EnergyRecord = {
    val date = readValidDate()
    val time = readValidTime()
    val energyType = readValidEnergyType()
    val generation = readValidGeneration()
    val storage = readValidStorage()
    val health = readValidHealth()
    val status = readValidStatus()

    EnergyRecord(date, time, energyType, generation, storage, health, status)
  }

  def main(args: Array[String]): Unit = {
    menuLoop()
  }
}
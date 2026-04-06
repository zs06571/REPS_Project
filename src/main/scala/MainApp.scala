package com.reps

import scala.io.StdIn.readLine

object MainApp {

  val filePath = "data/energy_data.csv"

  def showMenu(): Unit = {
    println("\n--- REPS Menu ---")
    println("1. View all records")
    println("2. Filter records")
    println("3. Sort records")
    println("4. Search by keyword")
    println("5. Analyze generation data")
    println("6. Generate alerts")
    println("7. View control recommendations")
    println("8. Add new record")
    println("9. Check file errors")
    println("10. Exit")
  }

  def filterMenu(records: List[EnergyRecord]): Unit = {
    println("\n--- Filter Menu ---")
    println("1. Filter by date")
    println("2. Filter by energy type")
    println("3. Filter by hour")
    println("4. Filter by month")
    println("5. Filter by date range")
    println("6. Filter by status")
    println("7. Back to main menu")

    val choice = readLine("Choose a filter option: ")

    choice match {
      case "1" =>
        val date = readValidDate()
        println(s"\nRecords for $date:")
        Utils.printRecords(QueryService.filterByDate(records, date))
        filterMenu(records)

      case "2" =>
        val energyType = readValidEnergyType()
        println(s"\nFiltered records for $energyType:")
        Utils.printRecords(QueryService.filterByEnergyType(records, energyType))
        filterMenu(records)

      case "3" =>
        val hour = readValidHour()
        println(s"\nRecords for hour $hour:")
        Utils.printRecords(QueryService.filterByHour(records, hour))
        filterMenu(records)

      case "4" =>
        val (month, year) = readValidMonthAndYear()
        println(s"\nRecords for month $month/$year:")
        Utils.printRecords(QueryService.filterByMonth(records, month, year))
        filterMenu(records)

      case "5" =>
        println("Enter start date:")
        val startDate = readValidDate()
        println("Enter end date:")
        val endDate = readValidDate()
        println(s"\nRecords from $startDate to $endDate:")
        Utils.printRecords(QueryService.filterByDateRange(records, startDate, endDate))
        filterMenu(records)

      case "6" =>
        val status = readValidStatus()
        println(s"\nFiltered records for status $status:")
        Utils.printRecords(QueryService.filterByStatus(records, status))
        filterMenu(records)

      case "7" =>
        menuLoop()

      case _ =>
        println("Invalid filter option.")
        filterMenu(records)
    }
  }

  def sortMenu(records: List[EnergyRecord]): Unit = {
    println("\n--- Sort Menu ---")
    println("1. Sort by generation ascending")
    println("2. Sort by generation descending")
    println("3. Sort by storage ascending")
    println("4. Sort by storage descending")
    println("5. Back to main menu")

    val choice = readLine("Choose a sort option: ")

    choice match {
      case "1" =>
        Utils.printRecords(QueryService.sortByGenerationAsc(records))
        sortMenu(records)

      case "2" =>
        Utils.printRecords(QueryService.sortByGenerationDesc(records))
        sortMenu(records)

      case "3" =>
        Utils.printRecords(QueryService.sortByStorageAsc(records))
        sortMenu(records)

      case "4" =>
        Utils.printRecords(QueryService.sortByStorageDesc(records))
        sortMenu(records)

      case "5" =>
        menuLoop()

      case _ =>
        println("Invalid sort option.")
        sortMenu(records)
    }
  }

  def readValidDate(): String = {
    val input = readLine("Enter date (DD/MM/YYYY): ")
    if (Validation.isValidDate(input)) input
    else {
      println("Invalid date. Please use DD/MM/YYYY. Day must be 01-31, month must be 01-12, and year must be between 1900 and 2100.")
      readValidDate()
    }
  }

  def readValidTime(): String = {
    val input = readLine("Enter time (HH:MM): ")
    if (Validation.isValidTime(input)) input
    else {
      println("Invalid time. Please use HH:MM. Hour must be between 00 and 23, and minute must be between 00 and 59.")
      readValidTime()
    }
  }

  def readValidHour(): String = {
    val input = readLine("Enter hour (HH): ")
    if (Validation.isValidHour(input)) input
    else {
      println("Invalid hour. Please enter a value between 00 and 23.")
      readValidHour()
    }
  }

  def readValidMonthAndYear(): (String, String) = {
    val month = readLine("Enter month (MM): ")
    val year = readLine("Enter year (YYYY): ")

    if (!Validation.isValidMonth(month)) {
      println("Invalid month. Please enter a value between 01 and 12.")
      readValidMonthAndYear()
    } else if (!Validation.isValidYear(year)) {
      println("Invalid year. Please enter a value between 1900 and 2100.")
      readValidMonthAndYear()
    } else {
      (month, year)
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
        filterMenu(records)

      case "3" =>
        sortMenu(records)

      case "4" =>
        val keyword = readLine("Enter keyword (examples: 12/04/2024, 08:00, Solar, Wind, Malfunction, Normal): ")
        println(s"\nSearch results for '$keyword':")
        Utils.printRecords(QueryService.searchByKeyword(records, keyword))
        menuLoop()

      case "5" =>
        println("\nGeneration analysis:")
        Utils.printAnalysis(AnalyticsService.analyzeGeneration(records))
        menuLoop()

      case "6" =>
        println("\nGenerated alerts:")
        val alerts = AlertService.generateAllAlerts(records)
        Utils.printAlerts(alerts)
        menuLoop()

      case "7" =>
        println("\nControl recommendations:")
        val alerts = AlertService.generateAllAlerts(records)
        val recommendations = ControlService.generateRecommendations(alerts)
        Utils.printRecommendations(recommendations)
        menuLoop()

      case "8" =>
        val record = createRecordFromInput()
        FileIO.appendRecord(filePath, record)
        println("New record added successfully.")
        menuLoop()

      case "9" =>
        println("\nChecking file parsing errors:")
        val errors = FileIO.loadErrors(filePath)
        Utils.printErrors(errors)
        menuLoop()

      case "10" =>
        println("Exiting REPS...")

      case _ =>
        println("Invalid menu option.")
        menuLoop()
    }
  }

  def main(args: Array[String]): Unit = {
    menuLoop()
  }
}
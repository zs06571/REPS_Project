package com.reps

object QueryService {

  def filterByDate(records: List[EnergyRecord], date: String): List[EnergyRecord] = {
    records.filter(_.date == date)
  }

  def filterByEnergyType(records: List[EnergyRecord], energyType: EnergyType): List[EnergyRecord] = {
    records.filter(_.energyType == energyType)
  }

  def filterByHour(records: List[EnergyRecord], hour: String): List[EnergyRecord] = {
    records.filter(_.time.startsWith(hour + ":"))
  }

  def filterByMonth(records: List[EnergyRecord], month: String, year: String): List[EnergyRecord] = {
    records.filter { record =>
      val parts = record.date.split("/")
      parts.length == 3 && parts(1) == month && parts(2) == year
    }
  }

  def filterByDateRange(records: List[EnergyRecord], startDate: String, endDate: String): List[EnergyRecord] = {
    def toTuple(date: String): (Int, Int, Int) = {
      val parts = date.split("/")
      (parts(2).toInt, parts(1).toInt, parts(0).toInt)
    }

    def isAfterOrEqual(a: (Int, Int, Int), b: (Int, Int, Int)): Boolean = {
      a._1 > b._1 ||
        (a._1 == b._1 && a._2 > b._2) ||
        (a._1 == b._1 && a._2 == b._2 && a._3 >= b._3)
    }

    def isBeforeOrEqual(a: (Int, Int, Int), b: (Int, Int, Int)): Boolean = {
      a._1 < b._1 ||
        (a._1 == b._1 && a._2 < b._2) ||
        (a._1 == b._1 && a._2 == b._2 && a._3 <= b._3)
    }

    val start = toTuple(startDate)
    val end = toTuple(endDate)

    records.filter { record =>
      val current = toTuple(record.date)
      isAfterOrEqual(current, start) && isBeforeOrEqual(current, end)
    }
  }

  def filterByWeek(records: List[EnergyRecord], startDate: String): List[EnergyRecord] = {
    val parts = startDate.split("/")
    val day = parts(0).toInt
    val month = parts(1).toInt
    val year = parts(2).toInt

    val start = java.time.LocalDate.of(year, month, day)
    val end = start.plusDays(6)

    val endDate =
      f"${end.getDayOfMonth}%02d/${end.getMonthValue}%02d/${end.getYear}%04d"

    filterByDateRange(records, startDate, endDate)
  }

  def filterByStatus(records: List[EnergyRecord], status: PlantStatus): List[EnergyRecord] = {
    records.filter(_.status == status)
  }

  def sortByActualGenerationAsc(records: List[EnergyRecord]): List[EnergyRecord] = {
    records.sortBy(_.actualGeneration)
  }

  def sortByActualGenerationDesc(records: List[EnergyRecord]): List[EnergyRecord] = {
    records.sortBy(_.actualGeneration)(Ordering[Double].reverse)
  }


  def searchByKeyword(records: List[EnergyRecord], keyword: String): List[EnergyRecord] = {
    val lowerKeyword = keyword.toLowerCase

    records.filter { record =>
      record.date.toLowerCase.contains(lowerKeyword) ||
        record.time.toLowerCase.contains(lowerKeyword) ||
        record.energyType.toString.toLowerCase.contains(lowerKeyword) ||
        record.status.toString.toLowerCase.contains(lowerKeyword) ||
        record.possibleCause.getOrElse("").toLowerCase.contains(lowerKeyword)
    }
  }

  def validForAnalysis(records: List[EnergyRecord]): List[EnergyRecord] = {
    records.filter(_.isValidForAnalysis)
  }
}
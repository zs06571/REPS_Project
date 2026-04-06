package com.reps

object QueryService {

  def filterByDate(records: List[EnergyRecord], date: String): List[EnergyRecord] = {
    records.filter(_.date == date)
  }

  def filterByEnergyType(records: List[EnergyRecord], energyType: EnergyType): List[EnergyRecord] = {
    records.filter(_.energyType == energyType)
  }

  def filterByStatus(records: List[EnergyRecord], status: PlantStatus): List[EnergyRecord] = {
    records.filter(_.status == status)
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
    def toNumber(date: String): Int = {
      val parts = date.split("/")
      val day = parts(0).toInt
      val month = parts(1).toInt
      val year = parts(2).toInt
      year * 10000 + month * 100 + day
    }

    val start = toNumber(startDate)
    val end = toNumber(endDate)

    records.filter { record =>
      val current = toNumber(record.date)
      current >= start && current <= end
    }
  }

  def sortByGenerationAsc(records: List[EnergyRecord]): List[EnergyRecord] = {
    records.sortBy(_.generation)
  }

  def sortByGenerationDesc(records: List[EnergyRecord]): List[EnergyRecord] = {
    records.sortBy(_.generation)(Ordering[Double].reverse)
  }

  def sortByStorageAsc(records: List[EnergyRecord]): List[EnergyRecord] = {
    records.sortBy(_.storage)
  }

  def sortByStorageDesc(records: List[EnergyRecord]): List[EnergyRecord] = {
    records.sortBy(_.storage)(Ordering[Double].reverse)
  }

  def searchByKeyword(records: List[EnergyRecord], keyword: String): List[EnergyRecord] = {
    records.filter { record =>
      record.date.contains(keyword) ||
        record.time.contains(keyword) ||
        record.energyType.toString.contains(keyword) ||
        record.status.toString.contains(keyword)
    }
  }

  def sortByField[A: Ordering](records: List[EnergyRecord], selector: EnergyRecord => A): List[EnergyRecord] = {
    records.sortBy(selector)
  }
}
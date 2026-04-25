package com.reps

import sttp.client3._
import io.circe._
import io.circe.parser._
import java.time.LocalDate

object ApiService {

  private val apiKey = "09d84ed10cb04ceca6f5e8e574973621"
  private val baseUrl = "https://data.fingrid.fi/api"

  private val solarForecastDatasetId = 248
  private val windActualDatasetId = 181
  private val hydroActualDatasetId = 191

  private val storageChargingDatasetId = 399
  private val storageDischargingDatasetId = 398
  private val storageCapacityDatasetId = 424

  def importLatestSolarData(): Either[String, EnergyRecord] = {
    for {
      raw <- fetchLatestJson(solarForecastDatasetId)
      parsed <- parseSingleValue(raw)
      record <- buildSolarRecord(parsed)
    } yield record
  }

  def importLatestWindData(): Either[String, EnergyRecord] = {
    for {
      raw <- fetchLatestJson(windActualDatasetId)
      parsed <- parseSingleValue(raw)
      record <- buildWindRecord(parsed)
    } yield record
  }

  def importLatestHydroData(): Either[String, EnergyRecord] = {
    for {
      raw <- fetchLatestJson(hydroActualDatasetId)
      parsed <- parseSingleValue(raw)
      record <- buildHydroRecord(parsed)
    } yield record
  }

  def importLatestStorageData(): Either[String, StorageRecord] = {
    for {
      chargingJson <- fetchLatestJson(storageChargingDatasetId)
      _ = pause(2500)
      dischargingJson <- fetchLatestJson(storageDischargingDatasetId)
      _ = pause(2500)
      capacityJson <- fetchLatestJson(storageCapacityDatasetId)

      chargingData <- parseSingleValue(chargingJson)
      dischargingData <- parseSingleValue(dischargingJson)
      capacityData <- parseSingleValue(capacityJson)

      record <- buildStorageRecord(chargingData, dischargingData, capacityData)
    } yield record
  }

  def importSolarByRange(startDate: String, endDate: String): Either[String, List[EnergyRecord]] = {
    for {
      raw <- fetchRangeJson(solarForecastDatasetId, startDate, endDate)
      parsed <- parseManyValues(raw)
      records <- sequence(parsed.map(buildSolarRecord))
    } yield records
  }

  def importWindByRange(startDate: String, endDate: String): Either[String, List[EnergyRecord]] = {
    for {
      raw <- fetchRangeJson(windActualDatasetId, startDate, endDate)
      parsed <- parseManyValues(raw)
      records <- sequence(parsed.map(buildWindRecord))
    } yield records
  }

  def importHydroByRange(startDate: String, endDate: String): Either[String, List[EnergyRecord]] = {
    for {
      raw <- fetchRangeJson(hydroActualDatasetId, startDate, endDate)
      parsed <- parseManyValues(raw)
      records <- sequence(parsed.map(buildHydroRecord))
    } yield records
  }

  private def buildSolarRecord(data: (String, String, Double)): Either[String, EnergyRecord] = {
    val (date, time, value) = data
    val hour = time.split(":")(0).toInt

    val isDaytime = hour >= 6 && hour <= 20

    val status =
      if (isDaytime && value < 50.0) LowOutput
      else Normal

    Right(
      EnergyRecord(
        date = date,
        time = time,
        energyType = Solar,
        actualGeneration = value,
        status = status,
        possibleCause = None,
        isValidForAnalysis = true
      )
    )
  }

  private def buildWindRecord(data: (String, String, Double)): Either[String, EnergyRecord] = {
    val (date, time, value) = data

    val status =
      if (value < 500.0) LowOutput
      else Normal

    Right(
      EnergyRecord(
        date = date,
        time = time,
        energyType = Wind,
        actualGeneration = value,
        status = status,
        possibleCause = None,
        isValidForAnalysis = true
      )
    )
  }

  private def buildHydroRecord(data: (String, String, Double)): Either[String, EnergyRecord] = {
    val (date, time, value) = data

    Right(
      EnergyRecord(
        date = date,
        time = time,
        energyType = Hydro,
        actualGeneration = value,
        status = ForecastUnavailable,
        possibleCause = None,
        isValidForAnalysis = true
      )
    )
  }

  private def buildStorageRecord(
                                  chargingData: (String, String, Double),
                                  dischargingData: (String, String, Double),
                                  capacityData: (String, String, Double)
                                ): Either[String, StorageRecord] = {
    val (date, time, chargingValue) = chargingData
    val (_, _, dischargingValue) = dischargingData
    val (_, _, capacityValue) = capacityData

    Right(
      StorageRecord(
        date = date,
        time = time,
        chargingPower = chargingValue,
        dischargingPower = dischargingValue,
        installedCapacity = capacityValue
      )
    )
  }

  private def pause(milliseconds: Long): Unit = {
    Thread.sleep(milliseconds)
  }

  private def fetchLatestJsonOnce(datasetId: Int): Either[String, String] = {
    val backend = HttpURLConnectionBackend()
    val request = basicRequest
      .get(uri"$baseUrl/datasets/$datasetId/data/latest")
      .header("x-api-key", apiKey)

    try {
      val response = request.send(backend)
      if (response.code.isSuccess) {
        response.body.left.map(err => s"API body error for dataset $datasetId: $err")
      } else {
        Left(s"API request failed for dataset $datasetId with status ${response.code}")
      }
    } catch {
      case e: Exception =>
        Left(s"API request exception for dataset $datasetId: ${e.getMessage}")
    } finally {
      backend.close()
    }
  }

  private def fetchLatestJson(datasetId: Int): Either[String, String] = {
    fetchLatestJsonOnce(datasetId) match {
      case r @ Right(_) => r
      case Left(err) if err.contains("status 429") =>
        pause(3000)
        fetchLatestJsonOnce(datasetId)
      case Left(err) => Left(err)
    }
  }

  private def fetchRangeJsonOnce(datasetId: Int, startDate: String, endDate: String): Either[String, String] = {
    val backend = HttpURLConnectionBackend()

    val startIso = toIsoStart(startDate)
    val endIso = toIsoEnd(endDate)

    val request = basicRequest
      .get(uri"$baseUrl/datasets/$datasetId/data?startTime=$startIso&endTime=$endIso&pageSize=1000")
      .header("x-api-key", apiKey)

    try {
      val response = request.send(backend)
      if (response.code.isSuccess) {
        response.body.left.map(err => s"API body error for dataset $datasetId: $err")
      } else {
        Left(s"API request failed for dataset $datasetId with status ${response.code}")
      }
    } catch {
      case e: Exception =>
        Left(s"API request exception for dataset $datasetId: ${e.getMessage}")
    } finally {
      backend.close()
    }
  }

  private def fetchRangeJson(datasetId: Int, startDate: String, endDate: String): Either[String, String] = {
    fetchRangeJsonOnce(datasetId, startDate, endDate) match {
      case r @ Right(_) => r
      case Left(err) if err.contains("status 429") =>
        pause(3000)
        fetchRangeJsonOnce(datasetId, startDate, endDate)
      case Left(err) => Left(err)
    }
  }

  private def parseSingleValue(raw: String): Either[String, (String, String, Double)] = {
    parse(raw).left.map(_.message).flatMap { json =>
      val cursor = json.hcursor
      for {
        startTime <- cursor.get[String]("startTime").left.map(_.message)
        value <- cursor.get[Double]("value").left.map(_.message)
        dateTime <- convertDateTime(startTime)
      } yield (dateTime._1, dateTime._2, value)
    }
  }

  private def parseManyValues(raw: String): Either[String, List[(String, String, Double)]] = {
    parse(raw).left.map(_.message).flatMap { json =>

      val itemsOpt =
        json.asArray.orElse(
          json.hcursor.downField("data").focus.flatMap(_.asArray)
        )

      itemsOpt match {
        case Some(items) =>
          sequence(
            items.toList.map { item =>
              val cursor = item.hcursor
              for {
                startTime <- cursor.get[String]("startTime").left.map(_.message)
                value <- cursor.get[Double]("value").left.map(_.message)
                dateTime <- convertDateTime(startTime)
              } yield (dateTime._1, dateTime._2, value)
            }
          )

        case None =>
          Left("API range response is neither a JSON array nor an object containing a 'data' array.")
      }
    }
  }

  private def convertDateTime(startTime: String): Either[String, (String, String)] = {
    val parts = startTime.split("T")
    if (parts.length != 2) {
      Left("Could not parse startTime from API response.")
    } else {
      val datePart = parts(0)
      val timePart = parts(1).take(5)
      val datePieces = datePart.split("-")

      if (datePieces.length != 3) {
        Left("Could not parse date from API response.")
      } else {
        val formattedDate = s"${datePieces(2)}/${datePieces(1)}/${datePieces(0)}"
        Right((formattedDate, timePart))
      }
    }
  }

  private def toIsoStart(date: String): String = {
    val parts = date.split("/")
    s"${parts(2)}-${parts(1)}-${parts(0)}T00:00:00Z"
  }

  private def toIsoEnd(date: String): String = {
    val parts = date.split("/")
    s"${parts(2)}-${parts(1)}-${parts(0)}T23:59:59Z"
  }

  private def sequence[A](items: List[Either[String, A]]): Either[String, List[A]] = {
    items.foldRight(Right(Nil): Either[String, List[A]]) {
      case (Right(value), Right(acc)) => Right(value :: acc)
      case (Left(err), _)             => Left(err)
      case (_, Left(err))             => Left(err)
    }
  }

  def debugSolarRangeCount(startDate: String, endDate: String): Either[String, Int] = {
    fetchRangeJson(solarForecastDatasetId, startDate, endDate)
      .flatMap(parseManyValues)
      .map(_.length)
  }
}
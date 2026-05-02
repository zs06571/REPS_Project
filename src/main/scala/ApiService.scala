// Shuo Zhao 002513249
// Hongyao Liu 002513919 
// Hongrui Zhang 002520436

package com.reps

// External references used in this file:
// SoftwareMill. (n.d.). sttp: The Scala HTTP client you always wanted! — sttp 4 documentation. https://sttp.softwaremill.com/en/latest/
// circe. (n.d.). Parsing JSON. http://circe.io/circe/parsing.html
// sttp is used for HTTP requests to the Fingrid API.
// circe is used for JSON parsing.

import sttp.client3._
import io.circe._
import io.circe.parser._
import java.time.LocalDate

object ApiService {

  // Read the API key from environment variables if available
  private val apiKey: String =
    sys.env.getOrElse("FINGRID_API_KEY", "09d84ed10cb04ceca6f5e8e574973621")

  // Base URL for Fingrid API
  private val baseUrl = "https://data.fingrid.fi/api"

  // Dataset IDs for energy sources
  private val solarForecastDatasetId = 248
  private val windActualDatasetId = 181
  private val hydroActualDatasetId = 191

  // Dataset IDs for storage data
  private val storageChargingDatasetId = 399
  private val storageDischargingDatasetId = 398
  private val storageCapacityDatasetId = 424

  // Import the latest solar record
  def importLatestSolarData(): Either[String, EnergyRecord] = {
    for {
      raw <- fetchLatestJson(solarForecastDatasetId)
      parsed <- parseSingleValue(raw)
      record <- buildSolarRecord(parsed)
    } yield record
  }

  // Import the latest wind record
  def importLatestWindData(): Either[String, EnergyRecord] = {
    for {
      raw <- fetchLatestJson(windActualDatasetId)
      parsed <- parseSingleValue(raw)
      record <- buildWindRecord(parsed)
    } yield record
  }

  // Import the latest hydro record
  def importLatestHydroData(): Either[String, EnergyRecord] = {
    for {
      raw <- fetchLatestJson(hydroActualDatasetId)
      parsed <- parseSingleValue(raw)
      record <- buildHydroRecord(parsed)
    } yield record
  }

  // Import the latest storage record
  def importLatestStorageData(): Either[String, StorageRecord] = {
    // Storage is built from charging, discharging, and capacity datasets
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

  // Import solar data for a date range
  def importSolarByRange(startDate: String, endDate: String): Either[String, List[EnergyRecord]] = {
    for {
      raw <- fetchRangeJson(solarForecastDatasetId, startDate, endDate)
      parsed <- parseManyValues(raw)
      records <- sequence(parsed.map(buildSolarRecord))
    } yield records
  }

  // Import wind data for a date range
  def importWindByRange(startDate: String, endDate: String): Either[String, List[EnergyRecord]] = {
    for {
      raw <- fetchRangeJson(windActualDatasetId, startDate, endDate)
      parsed <- parseManyValues(raw)
      records <- sequence(parsed.map(buildWindRecord))
    } yield records
  }

  // Import hydro data for a date range
  def importHydroByRange(startDate: String, endDate: String): Either[String, List[EnergyRecord]] = {
    for {
      raw <- fetchRangeJson(hydroActualDatasetId, startDate, endDate)
      parsed <- parseManyValues(raw)
      records <- sequence(parsed.map(buildHydroRecord))
    } yield records
  }

  // Build a solar record and assign status based on time-aware rules
  private def buildSolarRecord(data: (String, String, Double)): Either[String, EnergyRecord] = {
    val (date, time, value) = data
    val hour = time.split(":")(0).toInt

    val status =
      if (hour < 6 || hour > 20) {
        Normal
      } else if ((hour >= 6 && hour <= 8) || (hour >= 17 && hour <= 20)) {
        if (value < 20.0) LowOutput else Normal
      } else {
        if (value < 40.0) LowOutput else Normal
      }

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

  // Build a wind record and assign status using a fixed threshold
  private def buildWindRecord(data: (String, String, Double)): Either[String, EnergyRecord] = {
    val (date, time, value) = data

    val status =
      if (value < 300.0) LowOutput
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

  // Build a hydro record and assign status using a fixed threshold
  private def buildHydroRecord(data: (String, String, Double)): Either[String, EnergyRecord] = {
    val (date, time, value) = data

    val status =
      if (value < 700.0) LowOutput
      else Normal

    Right(
      EnergyRecord(
        date = date,
        time = time,
        energyType = Hydro,
        actualGeneration = value,
        status = status,
        possibleCause = None,
        isValidForAnalysis = true
      )
    )
  }

  // Build one storage record from three latest values
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

  // Pause between requests to reduce API pressure
  private def pause(milliseconds: Long): Unit = {
    Thread.sleep(milliseconds)
  }

  // Send one latest-data request
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

  // Retry once if the API returns rate limit status 429
  private def fetchLatestJson(datasetId: Int): Either[String, String] = {
    fetchLatestJsonOnce(datasetId) match {
      case r @ Right(_) => r
      case Left(err) if err.contains("status 429") =>
        pause(3000)
        fetchLatestJsonOnce(datasetId)
      case Left(err) => Left(err)
    }
  }

  // Send one range-data request
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

  // Retry once if the range request gets status 429
  private def fetchRangeJson(datasetId: Int, startDate: String, endDate: String): Either[String, String] = {
    fetchRangeJsonOnce(datasetId, startDate, endDate) match {
      case r @ Right(_) => r
      case Left(err) if err.contains("status 429") =>
        pause(3000)
        fetchRangeJsonOnce(datasetId, startDate, endDate)
      case Left(err) => Left(err)
    }
  }

  // Parse one latest JSON result into (date, time, value)
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

  // Parse many range JSON results into a list of (date, time, value)
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

  // Convert API time format into dd/MM/yyyy and HH:MM
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

  // Convert dd/MM/yyyy to ISO start time
  private def toIsoStart(date: String): String = {
    val parts = date.split("/")
    s"${parts(2)}-${parts(1)}-${parts(0)}T00:00:00Z"
  }

  // Convert dd/MM/yyyy to ISO end time
  private def toIsoEnd(date: String): String = {
    val parts = date.split("/")
    s"${parts(2)}-${parts(1)}-${parts(0)}T23:59:59Z"
  }

  // Turn List[Either[String, A]] to Either[String, List[A]]
  private def sequence[A](items: List[Either[String, A]]): Either[String, List[A]] = {
    items.foldRight(Right(Nil): Either[String, List[A]]) {
      case (Right(value), Right(acc)) => Right(value :: acc)
      case (Left(err), _)             => Left(err)
      case (_, Left(err))             => Left(err)
    }
  }
}

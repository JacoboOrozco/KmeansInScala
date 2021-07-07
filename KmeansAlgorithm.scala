import scala.collection.mutable.ArrayBuffer
import org.scalameter._
import scala.collection.parallel.CollectionConverters._



/*
USE THIS EXAMPLE WHEN USING MAPS
//      for((k,v)<- clusters){
//        println(k)
//        for(p <- v){
//          println("\t" + p.mkString(","))
//        }
      //}
 */

object KmeansAlgorithm
{
  //Global Variables

  // Points Quantity represents how many points we will have
  val pointsQuantity = 2000
  // Point Range represents the range a point may be assigned
  // i.e if pointRange = 100, this means that a points coordinate x may only be a number from 0 to 100.
  val pointRange = 100
  // Point Dimension represents how many dimensions the points will have
  val pointDimension = 100
  // K represents how many centroids we will have, therefore it also represents how many clusters there will be
  val k = 5
  // Epsilon represents a parameter that decides when the difference between the old centroids and the new centroids
  // is insignificant enough to not warrant more iterations.
  val epsilon = 0.5

  def main(args: Array[String]): Unit =
  {
    // pointsPopulation represents the matrix which will contain all the points
    // the matrix will be visualized like follows: the rows shall be a different point, the columns shall be the coordinates of each point
    var pointsMatrix = pointsPopulation()

    // centroidsMatrix represents the matrix which will contain all the centroids
    var centroidsMatrix = chooseCentroids(pointsMatrix)


    val time = config(
      Key.exec.benchRuns -> 20,
    ) withWarmer{
      new Warmer.Default
    } withMeasurer {
    new Measurer.IgnoringGC
    } measure {
    secuentialKmeans(pointsMatrix, centroidsMatrix)
    }
    println("Time to completion = " + time)
//    val t1 = System.nanoTime
//    secuentialKmeans(pointsMatrix, centroidsMatrix)
//    val duration1 = (System.nanoTime - t1) / 1e9d
//    println("Duration in nanoseconds of sequential execution: " + duration1)
  }

  def secuentialKmeans(pointsMatrix: Array[Array[Double]], centroidsMatrix: Array[Array[Double]]): Unit =
  {

    //PreviousSSE represents the previous SUM SQUARED ERROR which will be used to decide when the difference between the previous centroids
    //and the current centroids is small enough to not warrant another iteration.
    var previousSSE = 0.0
    //done represents the flag we will use in order to stop the while cycle
    var done = false
    println
    // pointsPopulation represents the matrix which will contain all the points.
    // The matrix will be visualized like follows:
    // The rows shall be a different points, the columns shall be the coordinates of each point
    // Here we show our matrix full of points
    println("Matrix of points")
    //printMatrix represents a function that will eliminate the need to re write code in order to print Array[Array[Double]] type variables
    //printMatrix(pointsMatrix)
    println
    // centroidsMatrix represents the matrix which will contain all the centroids, it will be visualized just as the pointsMatrix
    var centroidMatrix = centroidsMatrix
    //Here we show our matrix full of points
    println("Matrix of centroids")
    printMatrix(centroidMatrix)
    println
    // Iterations represents the quantity of times we found new centroids before the optimal ones were found
    var iterations = 1

    while(!done)
      {
        // Clusters represents the map which will contain:
        // The position of each centroid within the centroids matrix, along with a matrix filled with the points that belong with that centroid inside each cluster
        var clusters = pointsMatrix.groupBy(point => nearestCentroidClassification(point, centroidMatrix))
        //println("Clusters")
        //printMapClassification(clusters)
        println
        // quantityAndDistance represents and matrix which will contain:
        // Tuples which indicate the amount of points and their distance to each centroid within the clusters
        var quantityAndDistance = locationAndDistanceWithMap(clusters, centroidMatrix)
        var test =  removeNull(quantityAndDistance)
        println("------")
        println("Matrix of how many points belong to centroid and their accumulate distances")
        printTuple(test)
        // nearestCentroidDistanceAcum represents
        var nearestCentroidDistanceAcum = extractDistance(test)
        //println("Cumulative of distances closest to the centroid " + nearestCentroidDistanceAcum.mkString(" , "))
        var howManyPointsBelongToCentroid = extractQuantity(test)
        //println("How many points belong to the centroid " + howManyPointsBelongToCentroid.mkString(" , "))
        // Calculate the average -- error average
        var sse = totalError(nearestCentroidDistanceAcum, howManyPointsBelongToCentroid)
        println("Average error sum")
        println(sse)
        centroidMatrix = updateCentroids(pointsMatrix, clusters, howManyPointsBelongToCentroid)
        println("New matrix of centroids")
        printMatrix(centroidMatrix)
        var error = (sse-previousSSE).abs
        if(error < epsilon)
        {
          done = true
          println("It took " + iterations + " iterations to completion")
        }
        previousSSE = sse
        iterations += 1
      }
  }

  //This function prints matrices of points to save programing redundancy
  def printMatrix(matrix: Array[Array[Double]]): Unit =
  {
    matrix.foreach(row =>println("<" + row.mkString(",") + ">") )
  }

  def removeNull(quantityAndDistance: Array[(Int, Double)]): ArrayBuffer[(Int,Double)] =
  {
    val test = ArrayBuffer[(Int, Double)]()
    var rowPos= 0
    var newRowPos = 0
    for(rowPos <- 0 to k - 1)
    {
      if(quantityAndDistance(rowPos) == null)
      {
        var newRowPos = rowPos + 1
        if(newRowPos < k)
        {
          var closestcentroid = quantityAndDistance(newRowPos)
          test += closestcentroid
        }
      }else
      {
        var closestcentroid = quantityAndDistance(rowPos)
        test += closestcentroid
      }
    }
    test
  }

  //This function prints matrices of points to save programing redundancy
  def printTuple(matrix: ArrayBuffer[(Int, Double)]): Unit =
  {
    matrix.foreach(row =>println("<" + row._1 + ">" + "<" + row._2 + ">"))
  }

  def printMapClassification(clusters: Map[Int, Array[Array[Double]]]) =
    {
      clusters.keys.foreach (cluster => {
        println("Centroid Position = " + cluster)
        printMatrix(clusters(cluster))
      })
    }

  def printMapDistanceClassification(matrix: Array[Double]) =
  {
    matrix.foreach(row =>println("<" + row + ">") )
  }

  // The function pointsPopulation creates the matrix of points and returns it
  def pointsPopulation(): Array[Array[Double]] =
  {
    var emptyArray = Array.ofDim[Double](pointsQuantity, pointDimension)
    val random = scala.util.Random
    var matrixOfPoints = emptyArray.map(row => row.map(col => random.nextInt(pointRange).toDouble))
    matrixOfPoints
  }

  //This function chooses random centroid by selecting k random points from the pointsMatrix
  def chooseCentroids(pointsMatrix: Array[Array[Double]]): Array[Array[Double]] =
  {
    var centroidsMatrix = Array.ofDim[Double](k, pointDimension)
    var centroid = Array[Double]()
    var usedPoints = ArrayBuffer[Int]()
    val random = scala.util.Random
    //var matrixOfCentroids = emptyArray.map(row => differentCentroids(pointsMatrix))
    for(rowPos <- 0 to k - 1)
      {
        var randomNumber = random.nextInt(pointsMatrix.length)
        if(rowPos == 0)
          {
            centroid = pointsMatrix(randomNumber)
            centroidsMatrix(rowPos) = centroid
            usedPoints += randomNumber
          }
        if(rowPos != 0)
        {
          while (usedPoints.contains(randomNumber) == true)
          {
            randomNumber = random.nextInt(pointsMatrix.length)
            usedPoints += randomNumber
//            centroid = pointsMatrix(randomNumber)
//            centroidsMatrix(rowPos) = centroid
          }
          centroid = pointsMatrix(randomNumber)
          centroidsMatrix(rowPos) = centroid
        }
      }
      centroidsMatrix
  }


  def nearestCentroidClassification(point: Array[Double], centroidsMatrix: Array[Array[Double]]): (Int) =
  {
    var nearestDistance = secuentialDistance(point, centroidsMatrix(0))
    var nearestCentroid = 0
    var rowPos = 1

    while(rowPos < centroidsMatrix.length)
    {
      val distance = secuentialDistance(point, centroidsMatrix(rowPos))
      if(distance < nearestDistance)
      {
        nearestDistance = distance
        nearestCentroid = rowPos
      }
      rowPos = rowPos + 1
    }
    (nearestCentroid)
  }

  def nearestCentroidDistanceClassification(point: Array[Double], centroidsMatrix: Array[Array[Double]]): (Double) =
  {
    var nearestDistance = secuentialDistance(point, centroidsMatrix(0))
    var nearestCentroid = 0
    var rowPos = 1

    while(rowPos < centroidsMatrix.length)
    {
      val distance = secuentialDistance(point, centroidsMatrix(rowPos))
      if(distance < nearestDistance)
      {
        nearestDistance = distance
        nearestCentroid = rowPos
      }
      rowPos = rowPos + 1
    }
    (nearestDistance)
  }

  def locationAndDistanceWithMap(clusters: Map[Int, Array[Array[Double]]], centroidMatrix: Array[Array[Double]]): Array[(Int,Double)] =
    {
      var distancesAndHoyManyPoints = Array.ofDim[(Int, Double)](k)
      var distancesMatrix = Array.ofDim[Double](k)
      var distance = 0.0
      var rowPos = 0
      var centroid = Array[Double]()
      for((centroidRowPos, pointsMatrix)<- clusters)
      {
        centroid = centroidMatrix(centroidRowPos)
        rowPos = 0
//        println("------")
//        println("centroid = " + centroidRowPos)
//        println("<" +centroid.mkString(",") + ">")
//        println("Matrix of points that belong to centroid = " + centroidRowPos)
//        printMatrix(pointsMatrix)
//        println("------")
        while(rowPos < pointsMatrix.length)
          {
            distance = secuentialDistance(pointsMatrix(rowPos), centroid)
            //println("DISTANCE FROM POINT = " +rowPos + " TO CENTROID =" + centroidRowPos + " IS EQUAL TO = " +distance)
            rowPos += 1
            distancesMatrix(centroidRowPos) += distance
            var closestCentroid = (rowPos, distancesMatrix(centroidRowPos))
            distancesAndHoyManyPoints(centroidRowPos) = closestCentroid
          }

      }
      distancesAndHoyManyPoints
    }

  //This function will make the calculation for the sequential tests
  def secuentialDistance(point: Array[Double], centroid: Array[Double]): Double =
  {
    var distance = 0.0
    var colPos = 0


    while(colPos < point.length)
    {
      val dist = point(colPos) - centroid(colPos)
      distance += dist*dist
      colPos += 1
    }
    distance
  }

  def extractDistance(quantityAndDistance: ArrayBuffer[(Int, Double)]): Array[Double] =
    {
      var distanceAcumulate = Array.ofDim[Double](k)
      for(rowPos <- 0 to quantityAndDistance.length - 1)
        {
          distanceAcumulate(rowPos) = quantityAndDistance(rowPos)._2
        }
        distanceAcumulate
    }

  def extractQuantity(quantityAndDistance: ArrayBuffer[(Int, Double)]): Array[Int] =
    {
      var quantityOfPoints = Array.ofDim[Int](k)
      for(rowPos <- 0 to quantityAndDistance.length - 1)
      {
        quantityOfPoints(rowPos) = quantityAndDistance(rowPos)._1
      }
      quantityOfPoints
    }

  def totalError(nearestCentroidDistanceAcum: Array[Double], howManyPointsBelongToCentroid: Array[Int]): Double=
  {
    var cumulate = 0.0
    var averageDistanceToCentroid = divideArrays(nearestCentroidDistanceAcum, howManyPointsBelongToCentroid)

    for(i <- 0 to averageDistanceToCentroid.length -1)
    {
      cumulate +=  averageDistanceToCentroid(i)
    }
    cumulate
  }

  def divideArrays(nearestCentroidDistanceAcum: Array[Double], howManyPointsBelongToCentroid: Array[Int]): Array[Double] =
  {
    var r = Array.ofDim[Double](nearestCentroidDistanceAcum.length)
    var rowPos = 0
    var pos = 0
    for(rowPos <- 0 to nearestCentroidDistanceAcum.length -1)
    {
      if(nearestCentroidDistanceAcum(rowPos) == 0)
        {
          var pos = rowPos
          pos += 1
          if(pos < nearestCentroidDistanceAcum.length)
            {
              r(pos) = nearestCentroidDistanceAcum(pos) / howManyPointsBelongToCentroid(pos).toDouble
            }

        }else
        {
          r(rowPos) = nearestCentroidDistanceAcum(rowPos) / howManyPointsBelongToCentroid(rowPos).toDouble
        }
    }
    r
  }

  def updateCentroids(pointsMatrix: Array[Array[Double]], clusters: Map[Int, Array[Array[Double]]], howManyPointsBelongToCentroid: Array[Int]): Array[Array[Double]] =
  {
    var newCentroids = Array.ofDim[Double](k, pointDimension)
    var acumCoord = Array.ofDim[Double](k, pointDimension)
    var rowPos = 0
    var colPos = 0
    for ((centroidRowPos, pointsMatrix) <- clusters)
    {
      while (colPos < pointDimension)
      {
        while (rowPos < pointsMatrix.length)
        {
          acumCoord(centroidRowPos)(colPos) += pointsMatrix(rowPos)(colPos)
          rowPos += 1
        }
        rowPos = 0
        colPos += 1
      }
      colPos = 0
    }

    for(pointRowPos <- 0 to k - 1)
      {
        newCentroids(pointRowPos) = divideArrayByInt(acumCoord(pointRowPos), howManyPointsBelongToCentroid(pointRowPos))
      }
    newCentroids
      //var centroid = calculateNewCentroids(pointsMatrix, centroidRowPos,howManyPointsBelongToCentroid(centroidRowPos))
//      println("----------------")
//      println("New Centroid")
//      centroid.foreach(row =>println("<" + row.mkString(",") + ">") )
  }

  def divideArrayByInt(acumCoord: Array[Double], howManyPointsBelongToCentroid: Int): Array[Double]=
  {
    var newCentroid = acumCoord.map(_ /howManyPointsBelongToCentroid.toDouble)
    newCentroid
  }
}


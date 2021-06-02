import scala.collection.mutable.ArrayBuffer

//This code is an implementation of the Kmeans Algorithm
//The implementation process will be divided into three steps
//1.Initialization:
// In this first phase we must: create our points, choose our K centroids at random from this points list.
//2.Assignment:
// In this second phase we must: create our K clusters by associating each point with the nearest centroid
//3.Update:
// In this third phase we must: Find the new centroids based on the clusters


object KmeansAlgorithm
{
  //Global Variables

  // Points Quantity represents how many points we will have
  val pointsQuantity = 50
  // Point Range represents the range a point may be assigned
  // i.e if pointRange = 100, this means that a points coordinate x may only be a number from 0 to 100.
  val pointRange = 20
  // Point Dimension represents how many dimensions the points will have
  val pointDimension = 10
  // K represents how many centroids we will have, therefore it also represents how many clusters there will be
  val k = 3
  // Epsilon represents a parameter that decides when the difference between the old centroids and the new centroids
  // is insignificant enough to not warrant more iterations.
  val epsilon = 2.0

  def main(args: Array[String]): Unit =
    {
      //PreviousSSE represents the previous SUM SQUARED ERROR which will be used to decide when the difference between the previous centroids
      // abd tge current centroids is small enough to not warrant another iteration
      var previousSSE = 0.0
      var done = false
      //1.Initialization:
      // In this first phase we must: create our points, choose our K centroids at random from this points list.

      println("Here begins the Initialization phase")
      println("-----------------------------------------------------------------------------------------------------------")
      // pointsPopulation represents the matrix which will contain all the points
      var pointsMatrix = pointsPopulation(pointsQuantity, pointRange, pointDimension)
      //Here we show our matrix full of points
      println("Matrix of points")
      printMatrix(pointsMatrix)
      println(pointsMatrix.length)

      println

      // centroidsMatrix represents the matrix which will contain all the centroids
      var centroidsMatrix = chooseCentroids(pointsMatrix)
      //Here we show our matrix full of points
      println("Matrix of centroids")
      printMatrix(centroidsMatrix)

      println

      println("Here ends the Initialization phase")
      println("-----------------------------------------------------------------------------------------------------------")

      println
      var iterations = 0

      while(!done)
        {
          //This Array will hold the distances between
          var nearestCentroidDistanceAcum = Array.ofDim[Double](k)

          //This Array will hold the number of points that belong to a centroid
          var howManyPointsBelongToCentroid = Array.ofDim[Int](k)

          //Here we will calculate the nearest centroid for each point in the pointsMatrix matrix
          var nearestToCentroid = nearestToCentroidClassification(pointsMatrix, centroidsMatrix)
          //printBufferTuple(nearestToCentroid)

          for(pointRowPos <- 0 to nearestToCentroid.length -1)
            {
              // colPos represents the position in which we must add
              var centroidRowPos = nearestToCentroid(pointRowPos)._1
              nearestCentroidDistanceAcum(centroidRowPos) = nearestCentroidDistanceAcum(centroidRowPos) + nearestToCentroid(centroidRowPos)._2
              howManyPointsBelongToCentroid(centroidRowPos) = howManyPointsBelongToCentroid(centroidRowPos) + 1
            }
          println("Cumulative of distances closest to the centroid " + nearestCentroidDistanceAcum.mkString(" , "))
          println("How many points belong to the centroid " + howManyPointsBelongToCentroid.mkString(" , "))

          // Calculate the average -- error average
          var sse = totalError(nearestCentroidDistanceAcum, howManyPointsBelongToCentroid)
          println("Average error sum")
          println(sse)
          centroidsMatrix = updateCentroidsMatrix(pointsMatrix, howManyPointsBelongToCentroid, nearestToCentroid)
          println("New Centroids")
          printMatrix(centroidsMatrix)
          println("-----------------------------------------------------------------------------------------------------------")

          var error = (sse-previousSSE).abs

          if(error < epsilon)
            {
              done = true
              println("It took " + iterations +" iterations to completion")
            }
            previousSSE = sse
          iterations += iterations + 1
        }

    }

  //This function prints matrices of points to save programing redundancy
  def printMatrix(matrix: Array[Array[Double]]): Unit =
    {
      for(rowPos <- 0 to matrix.length - 1)
        {
          for(columnPos <- 0 to pointDimension - 1)
            {
              print("(" + matrix(rowPos)(columnPos)+") ")
            }
            println
        }
    }

  def printBufferTuple(matrix: ArrayBuffer[(Int, Double)]): Unit =
    {
      for(rowPos <- 0 to matrix.length - 1)
        {
          println(matrix(rowPos)._1 + "," + matrix(rowPos)._2)
        }
    }

  // The function pointsPopulation creates the matrix of points and returns it
  def pointsPopulation(pointsQuantity: Int, pointRange: Int, pointDimension: Int): Array[Array[Double]] =
    {
      var matrixOfPoints = Array.ofDim[Double](pointsQuantity, pointDimension)
      val random = scala.util.Random

      for(rowPos <- 0 to pointsQuantity - 1)
        {
          for(columnPos <- 0 to pointDimension - 1)
            {
              var randomNumber = random.nextInt(pointRange).toDouble
              matrixOfPoints(rowPos)(columnPos) = randomNumber
            }
        }
        matrixOfPoints
    }

  //This function chooses random centroid by selecting k random points from the pointsMatrix
  def chooseCentroids(pointsMatrix: Array[Array[Double]]): Array[Array[Double]] =
    {
      var matrixOfCentroids = Array.ofDim[Double](k, pointDimension)
      val random = scala.util.Random

      for(rowPos <- 0 to k - 1)
        {
          var randomNumber = random.nextInt(pointsMatrix.length)
          var centroid = pointsMatrix(randomNumber)
          matrixOfCentroids(rowPos) = centroid
        }
      matrixOfCentroids
    }

  // This function will return an Array that will contain
  def nearestToCentroidClassification(pointsMatrix: Array[Array[Double]], centroidsMatrix: Array[Array[Double]]): ArrayBuffer[(Int, Double)] =
  {
    //Calculate the nearest centroid to each point in pointsMatrix
    var nearestToCentroid = ArrayBuffer[(Int, Double)]()

    for(rowPos <- 0 to pointsMatrix.length -1)
      {
        //closestCentroid represents a tuple which gives us the row & distance of the nearest centroid to a specific point located on row: rowPos
        var closestCentroid = nearestCentroid(pointsMatrix(rowPos), centroidsMatrix)
        println("Closest centroid row & distance to point: " + "(" + rowPos +") ")
        println(closestCentroid)
        nearestToCentroid += closestCentroid
      }
      nearestToCentroid
  }

  def nearestCentroid(point: Array[Double], centroidsMatrix: Array[Array[Double]]): (Int, Double) =
    {
      var nearestDistance = eucledeanDistance(point, centroidsMatrix(0))
      var nearestCentroid = 0
      var rowPos = 1

      while(rowPos < centroidsMatrix.length)
        {
          val distance = eucledeanDistance(point, centroidsMatrix(rowPos))
          if(distance < nearestDistance)
            {
              nearestDistance = distance
              nearestCentroid = rowPos
            }
            rowPos = rowPos + 1
        }
      (nearestCentroid, nearestDistance)
    }


  //This function will take all the elements necessary to calculate the Euclidean distance in a concurrent way
  //It returs a Double that represents THE TOTAL DISTANCE BETWEEN TWO POINTS
  def eucledeanDistance(point: Array[Double], centroid: Array[Double]): Double =
  {
    var start = 0
    var end = point.length
    //Minimin length represents the limit that decides weather it will be processed in a sequential or concurrent way
    var minimunLength = 10
    partialDistance(point, centroid, start, end, minimunLength)
  }

  //This function will
  def partialDistance(point: Array[Double], centroid: Array[Double], start: Int, end: Int, minimumLength: Int): Double =
    {
      var distance = 0.0
      var rowPos = start

      var intervalLength = end - start

      //We will enter this if when we consider that it's better to have a sequential calculation of our Array or Segment
      if(intervalLength < minimumLength)
        {
          while(rowPos < end)
            {
              val dist = point(rowPos) - centroid(rowPos)
              distance += dist*dist
              rowPos = rowPos + 1
            }
            distance
        }
      //If the Array or Segment is longer than our limit we must split it
      else
      {
        val middle = start + (end-start) / 2
        var firstSegment = partialDistance(point, centroid, start, middle, minimumLength)
        var secondSegment = partialDistance(point, centroid, middle + 1, end, minimumLength)
        var totalDistance = firstSegment - secondSegment
        totalDistance
      }

    }

  def totalError(nearestCentroidDistanceAcum: Array[Double], howManyPointsBelongToCentroid: Array[Int]): Double=
    {
      var cumulate = 0.0
      var averageDistanceToCentroid = divideArrays(nearestCentroidDistanceAcum, howManyPointsBelongToCentroid)

      for(i <- 0 to averageDistanceToCentroid.length -1)
        {
          cumulate = cumulate + averageDistanceToCentroid(i)
        }
        cumulate
    }

  def divideArrays(a: Array[Double], b: Array[Int]): Array[Double] =
    {
      var r = Array.ofDim[Double](a.length)
      for(i <- 0 to a.length -1)
        {
          r(i) = a(i) / b(i).toDouble
        }
        r
    }

  def updateCentroidsMatrix(pointsMatrix: Array[Array[Double]], howManyPointsBelongToCentroid: Array[Int], nearestToCentroidClassification: ArrayBuffer[(Int, Double)]):Array[Array[Double]]=
    {
      var newCentroids = Array.ofDim[Double](k, pointDimension)
      var acumCoord = Array.ofDim[Double](k, pointDimension)

      for(rowPos <- 0 to pointsMatrix.length - 1)
        {
          //To which cluster the points belong to
          var centroidRowPos = nearestToCentroidClassification(rowPos)._1

          for(colPos <- 0 to pointDimension - 1)
            {
              acumCoord(centroidRowPos)(colPos) = acumCoord(centroidRowPos)(colPos) + pointsMatrix(rowPos)(colPos).toDouble
            }
        }
        for(rowPos <- 0 to k - 1)
          {
            newCentroids(rowPos) = divideArrayByInt(acumCoord(rowPos), howManyPointsBelongToCentroid(rowPos))
          }
          newCentroids
    }

  def divideArrayByInt(a: Array[Double], b: Int): Array[Double]={
    var r = a.map(_ /b.toDouble)
    r
  }
}
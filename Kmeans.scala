import scala.collection.mutable.ArrayBuffer
import scala.math.sqrt
import scala.util.Random

object Kmeans{
  def main(args: Array[String]): Unit ={

  }
  var punto1 = Array(2.0,4.0)
  println("Este es el punto 1: ")
  println("("+punto1.mkString(",")+")")
  var punto2 = Array(3.0,1.0)
  println("Este es el punto 2: ")
  println("("+punto2.mkString(",")+")")
  var punto3 = Array(5.0,8.0)
  println("Este es el punto 3: ")
  println("("+punto3.mkString(",")+")")
  var punto4 = Array(6.0,4.0)
  println("Este es el punto 4: ")
  println("("+punto4.mkString(",")+")")
  var listaPuntos = Array(punto1, punto2, punto3, punto4)
  println("Esta es la lista de puntos: ")
  for(pos <- 0 until listaPuntos.length ){
    println("("+listaPuntos(pos).mkString(",")+")")
  }
  println("Este es el centroide 1, seleccionado al azar de la lista de puntos: ")
  val centroide1 = Random.shuffle(listaPuntos.toList).head
  println("("+centroide1.mkString(",")+")")
  var centroideTemporal = Random.shuffle(listaPuntos.toList).head
  while(centroide1 == centroideTemporal){
    centroideTemporal = Random.shuffle(listaPuntos.toList).head
  }
  val centroide2 = centroideTemporal
  println("Este es el centroide 2, seleccionado al azar de la lista de puntos: ")
  println("("+centroide2.mkString(",")+")")
  val distanciaACentroide1 = distanciaPuntoCentroide(listaPuntos, centroide1)
  println("El array de distancias al centroide 1 en orden de puntos es: ")
  println("("+distanciaACentroide1.mkString(",")+")")
  val distanciaACentroide2 = distanciaPuntoCentroide(listaPuntos, centroide2)
  println("El array de distancias al centroide 2 en orden de puntos es: ")
  println("("+distanciaACentroide2.mkString(",")+")")
  //gruposCentroidesyPuntos(distanciaACentroide1, distanciaACentroide2, listaPuntos, centroide1, centroide2)
//  var grupo1 = ArrayBuffer[Array[Array[Double]]] ()
//  var grupo2 = Array[Array[Double]]()
//  grupo1(0) +: Array(centroide1)
//  grupo2 +: centroide2
//  println("El grupo 1:")
//  println(grupo1.mkString(","))
//  for(pos <- 0 until grupo1.length ){
//    println("("+grupo1(pos).mkString(",")+")")
//  }
  //val grupo = grupo1(distanciaACentroide1, distanciaACentroide2, listaPuntos, centroide1)


  var cluster1 = clusters(distanciaACentroide1, distanciaACentroide2, listaPuntos, centroide1)
  println("El cluster 1 es: ")
  for(pos <- 0 until cluster1.length ){
    println("("+cluster1(pos).mkString(",")+")")
  }
  var cluster2 = clusters(distanciaACentroide1, distanciaACentroide2, listaPuntos, centroide2)
  println("El cluster 2 es: ")
  for(pos <- 0 until cluster2.length ){
    println("("+cluster2(pos).mkString(",")+")")
  }







  def clusters(distanciaACentroide1: Array[Double], distanciaACentroide2: Array[Double],
                        listaPuntos: Array[Array[Double]], centroide1: Array[Double]): ArrayBuffer[Array[Double]] ={
    val grupo1 = ArrayBuffer[Array[Double]]()
    grupo1 += centroide1
    var pos = 0
    while(pos < listaPuntos.length){
      if(distanciaACentroide1(pos) <= distanciaACentroide2(pos)){
        grupo1 += listaPuntos(pos)
      }
      pos = pos + 1
    }
    grupo1
  }


  def distanciaPuntoCentroide(listaPuntos: Array[Array[Double]], centroide: Array[Double]):Array[Double]={
    var i = 0
    var distanciaPtCent: Array[Double] = new Array[Double](listaPuntos.length)
    var distanciaCoordenada = 0.0
    while(i < listaPuntos.length){
      distanciaCoordenada = distance(listaPuntos(i),centroide)
      var k = i + 1
      //println("Este es el valor de la distancia del punto (" + listaPuntos(i).mkString(",") + ")" +
      //  " al centroide (" + centroide.mkString(",") +")")
      //println(distanciaCoordenada)
      distanciaPtCent(i) = distanciaCoordenada
      i = i + 1
    }
    distanciaPtCent

  }


  def distance(Punto:Array[Double],Centroide:Array[Double]):Double = {
    var dist = 0.0
    var i = 0
    while(i < Punto.length){
      val d = Punto(i)-Centroide(i)
      dist += d*d
      var k = i + 1
      //println("Este es el valor de la distancia parcial del punto (" + Punto.mkString(",") +
      //  ") al centroide: (" + Centroide.mkString(",") + ")")
      //println(dist)
      i = i+1
    }
    sqrt(dist)
  }

  /*
  def gruposCentroidesyPuntos(distanciaCentroide1: Array[Double], distanciaCentroide2: Array[Double],
                              listaPuntos: Array[Array[Double]], centroide1:Array[Double], centroide2:Array[Double]): Unit ={
    var i = 0
    var k = 1
    var grupo1 = Array[Array[Double]]()
    var grupo2 = Array[Array[Double]]()
    var longitud = grupo1.length
    centroide1 +: grupo1
    centroide2 +: grupo2
    while(i < distanciaCentroide1.length){
      if(distanciaCentroide1(i) > distanciaCentroide2(i)){
        grupo2(k)=listaPuntos(i)
        i = i+1
        k = k+1
      }
      if(distanciaCentroide1(i) < distanciaCentroide2(i)){
        grupo1(k)=listaPuntos(i)
        i = i+1
        k = k+1
      }
      if(distanciaCentroide1(i) == distanciaCentroide2(i)){
        grupo1(k)=listaPuntos(i)
        grupo2(k)=listaPuntos(i)
        i = i+1
        k = k+1
      }

    }
    println("Grupo 1:")
    println("Centroide 1:("+grupo1(0).mkString(",")+")")
    println("Puntos:(")
    for(pos <- 1 to grupo1.length){
      print(grupo1(pos).mkString(","))
    }
    print(")")
  }

 */


}
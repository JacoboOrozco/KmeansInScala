import scala.util.Random
object Algoritmo{
  def main(args: Array[String]): Unit ={
    val punto1 = Punto(50,50)
    val punto2 = Punto(55,55)
    val punto3 = Punto(45,50)
    val punto4 = Punto(95,100)
    val punto5 = Punto(100,95)
    val punto6 = Punto(98,100)
    val todos_Los_Puntos = List(punto1,punto2,punto3,punto4,punto5,punto6)
    println("--------------------------------")
    println("Lista de todos los puntos: ")
    println(todos_Los_Puntos)
    val grupos = gruposIniciales(todos_Los_Puntos, 2)
    println("--------------------------------")
    println("Grupos iniciales seleccionados al azar de la lista de puntos: ")
    println(grupos)
    println("--------------------------------")
    println("Asignacion de los puntos a los centroides basado en la distancia euclidiana: ")
    val grupos_Asignados = asignacion(todos_Los_Puntos, grupos)
    println(grupos_Asignados)
    println("--------------------------------")
    println("Volviendo a acomodar los centroides basado en el promedio de las componentes de los puntos: ")
    val reCentrado = volverACentrar(grupos_Asignados)
    println(reCentrado)
    println("--------------------------------")
    println("Volver a asignar grupos basado en los nuevos centroides: ")
    val nuevosCentroides = reCentrado.groupBy(centroide => reCentrado.map(_.centroide))
    println(nuevosCentroides)
    //val iteracion2 = asignacion(todos_Los_Puntos, nuevosCentroides)

  }
  case class Punto(x: Int, y:Int){
    def distanciaA(otro: Punto) =
      math.sqrt(math.pow(otro.x - this.x, 2)+ math.pow(otro.y - this.y, 2))
  }
  case class Grupo(centroide: Punto, puntos: List[Punto] = Nil)
  case class LoteGrupos(grupos: List[Grupo])

  def asignacion(todos_Los_Puntos:List[Punto], grupos:List[Grupo]):List[Grupo] = {
    val puntosPorGrupo = todos_Los_Puntos.groupBy(punto=> grupos.minBy(_.centroide.distanciaA(punto)))
    grupos.map(grupo => grupo.copy(puntos = puntosPorGrupo.getOrElse(grupo, Nil)))
  }

  def gruposIniciales(todos_Los_Puntos:List[Punto], k:Int):List[Grupo] =
    Random.shuffle(todos_Los_Puntos).take(k).map(punto => Grupo(punto))

  def volverACentrar(grupos:List[Grupo]): List[Grupo] =
    grupos.map
    {
      grupo =>
        val xProm = grupo.puntos.map(_.x).sum / grupo.puntos.size
        val yProm = grupo.puntos.map(_.y).sum / grupo.puntos.size
        grupo.copy(centroide = Punto(xProm, yProm))
    }

}
package relation
package compiler

import scala.collection.mutable

import ch.epfl.data.sc.pardis
import pardis.optimization.RecursiveRuleBasedTransformer
import pardis.quasi.TypeParameters._
import pardis.types._
import PardisTypeImplicits._
import pardis.ir._

import relation.deep.RelationDSLOpsPackaged
import relation.shallow._  

class ColumnStoreLowering(override val IR: RelationDSLOpsPackaged, override val schemaAnalysis: SchemaAnalysis) extends RelationLowering(IR, schemaAnalysis) {
  import IR.Predef._
  
  type Col = Array[String]
  type LoweredRelation = Rep[Array[Col]]
  
  def relationScan(scanner: Rep[RelationScanner], schema: Schema, size: Rep[Int], resultSchema: Schema): LoweredRelation = {
	val nbColumn = schema.size
	dsl"""
		val bigArray = new Array[Col]($nbColumn)
		for (i <- 0 until $nbColumn){
			bigArray(i) = new Array[String]($size)
		}
		var i = 0
		while($scanner.hasNext){
			for (column <- $schema.columns){
				bigArray($schema.indexOf(column))(i) = $scanner.next_string()
			}
			i = i + 1
		}
		bigArray
    """
  }
  
  def relationProject(relation: Rep[Relation], schema: Schema, resultSchema: Schema): LoweredRelation = {
    val arr = getRelationLowered(relation)
    val oldSchema = getRelationSchema(relation)
    def nbColumn = schema.size
    dsl"""
		val newArr = new Array[Col]($nbColumn)
		var i = 0
		for (colName <- $oldSchema.columns){
			for (col <- $schema.columns){
				if (colName == col){
					newArr(i) = $arr($oldSchema.indexOf(colName))
					i = i + 1
				}
			}
			
		}
		newArr
    """
  }
  
  def relationSelect(relation: Rep[Relation], field: String, value: Rep[String], resultSchema: Schema): LoweredRelation = {
    val arr = getRelationLowered(relation)
    val schema = getRelationSchema(relation)
    val index = schema.indexOf(field)
    val nbColumn = schema.size
	dsl"""
		var size = 0
		for (elem <- $arr($index)){
			if (elem == $value){
				size = size + 1
			}
		}
		val newArray = new Array[Col]($nbColumn)
		for (i <- 0 until $nbColumn){
			newArray(i) = new Array[String](size)
		}
		var k = 0
		for (i <- 0 until $arr(index).length){
			if ($value == $arr($index)(i)){
				for (j <- 0 until $nbColumn){
					newArray(j)(k) = $arr(j)(i)
				}
				k = k + 1
			}
		}
		newArray
     """
  }
  
  def relationJoin(leftRelation: Rep[Relation], rightRelation: Rep[Relation], leftKey: String, rightKey: String, resultSchema: Schema): LoweredRelation = {
    ??? // TODO
  }
  
  def relationPrint(relation: Rep[Relation]): Unit = {
    ??? // TODO
  }
  
}

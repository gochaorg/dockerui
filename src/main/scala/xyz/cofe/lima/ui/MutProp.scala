package xyz.cofe.lima.ui

case class MutProp(name:String, reader:()=>String, writer:String=>Unit)

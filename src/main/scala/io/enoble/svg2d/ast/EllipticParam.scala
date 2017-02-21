package io
package enoble
package svg2d
package ast

import io.enoble.svg2d.ast.InitialPath.Coords

case class EllipticParam(r: Coords, rotX: Double, largeArc: Boolean, sweep: Boolean, p: Coords)

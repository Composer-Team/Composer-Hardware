package composer.Generation

import composer.ComposerBuild
import firrtl._
import firrtl.ir.DefInstance
import os._

import java.io.FileWriter

class ExportCSymbolTransform extends Transform {
  override def inputForm: CircuitForm = ChirrtlForm

  override def outputForm: CircuitForm = ChirrtlForm

  final def getModulePath(modName: String, circuit: firrtl.ir.Circuit, path: Seq[String]): Seq[String] = {
    if (modName == circuit.main) return path :+ circuit.main
    else {
      circuit.modules.filter(_.isInstanceOf[firrtl.ir.Module]).map(_.asInstanceOf[firrtl.ir.Module]) foreach { mod =>
        mod.body.foreachStmt {
          case di: DefInstance =>
            if (di.module == modName) {
              return getModulePath(mod.name, circuit, path :+ di.name)
            }
          case _ => ;
        }
      }
    }
    path
  }

  override protected def execute(state: CircuitState): CircuitState = {
    // scrape hierarchy to get module path
    val cwd = ComposerBuild.composerGenDir
    if (!os.exists(Path(cwd))) {
      os.makeDir(Path(cwd))
    }
    val f = new FileWriter(cwd + "/CAnnotations.json")
    val (tgts_, annos_filter) = state.annotations.partition(p => p.isInstanceOf[ExportCSymbolAnnotation])
    val tgts = tgts_.map(_.asInstanceOf[ExportCSymbolAnnotation])
    val uniques = tgts.map(_.target.name).distinct
    val lines = uniques map { tgt_name =>
      val tgt_set = tgts.filter(_.target.name == tgt_name)

      val names = tgt_set map { tgt =>
        f"\"${getModulePath(tgt.target.module, state.circuit, Seq(tgt.target.name)).reverse.reduce(_ + "/" + _)}\""
      }
      val list = names.reduce(_ + ", " + _)
      val objective = tgt_set(0).designObjective
      f"\t\"$tgt_name\" : {\n\t\t\"paths\" : [ $list ],\n\t\t\"objective\": ${objective.getObjective}\n\t}"
    }
    f.write(f"{\n${lines.reduce(_ + ",\n" + _)}\n}")
    f.close()
    CircuitState(state.circuit, state.form, AnnotationSeq(annos_filter), state.renames)
  }
}


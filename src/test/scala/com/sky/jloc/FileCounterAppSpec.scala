package com.sky.jloc

import org.scalatest._

class FileCounterAppSpec extends FunSpec with Matchers {

    val filecountDir = "/home/lachatak/Documents/workspaces/pet/jloc/src/test/resources/filecount"
    val minionsDir = "/home/lachatak/Documents/workspaces/pet/jloc/src/test/resources/minions"
    val nofilesDir = "/home/lachatak/Documents/workspaces/pet/jloc/src/test/resources/nofiles"

    describe("FileCounterApp getFileList") {
        it("should throw IllegalArgumentException if the file doesn't exists"){
            intercept[IllegalArgumentException] {
                FileCounterApp.fileList("tmp")
            }
        }

        it("should throw IllegalArgumentException if the path is null"){
            intercept[IllegalArgumentException] {
                FileCounterApp.fileList(null)
            }
        }
    }

    describe("FileCounterApp countFiles") {
        it("should print 0 on empty directories"){
            FileCounterApp.countFiles(nofilesDir) shouldBe 0
        }

        it("should print 12 on filecount directories"){
            FileCounterApp.countFiles(filecountDir) shouldBe 12
        }

        it("should print 62 on miniouns directories" ){
            FileCounterApp.countFiles(minionsDir) shouldBe 62
        }
    }

    describe("FileCounterApp countLinse") {
        it("should print 0 on empty directories" ){
            FileCounterApp.countLines(nofilesDir) shouldBe 0
        }

        it("should print 16 on filecount directories"){
            FileCounterApp.countLines(filecountDir) shouldBe 16
        }

        it("should print 4034 on minionsDir directories"){
            FileCounterApp.countLines(minionsDir) shouldBe 4034
        }
    }

    describe("FileCounterApp fileDetails") {
        it("should print Details(39,435,255,1978) on filecount directories"){
            FileCounterApp.fileDetails(minionsDir, "java", line => line.startsWith("/*") || line.startsWith("*/") || line.startsWith("*")) shouldBe FileCounterApp.FileDetails(39,435,255,1978)
        }
    }
}

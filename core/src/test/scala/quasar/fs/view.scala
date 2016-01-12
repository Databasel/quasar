package quasar.fs

import quasar.Predef._

import quasar._
import LogicalPlan.{Free => LPFree, _}
import quasar.effect._
import quasar.fp._
import quasar.recursionschemes._
import quasar.std.StdLib._, set._

import monocle.macros.GenLens
import org.specs2.mutable._
import org.specs2.ScalaCheck
import pathy.{Path => PPath}, PPath._
import pathy.scalacheck.PathyArbitrary._
import scalaz._, Scalaz._
import org.scalacheck.{Arbitrary, Gen}

class ViewFSSpec extends Specification with ScalaCheck with TreeMatchers {
  import TraceFS._
  import FileSystemError._

  val query  = QueryFile.Ops[FileSystem]
  val read   = ReadFile.Ops[FileSystem]
  val write  = WriteFile.Ops[FileSystem]
  val manage = ManageFile.Ops[FileSystem]

  case class VS(seq: Long, handles: ViewHandles)
  val _seq = GenLens[VS](_.seq)
  val _handles = GenLens[VS](_.handles)

  implicit val arbLogicalPlan: Arbitrary[Fix[LogicalPlan]] = Arbitrary(Gen.const(Read(Path("/zips"))))

  type VSF[F[_], A] = StateT[F, VS, A]
  type VST[A] = VSF[Trace, A]

  def traceViewFs(nodes: Map[ADir, Set[Node]]): ViewFileSystem ~> VST =
    interpretViewFileSystem[VST](
      KeyValueStore.stateKeyValueStore[Trace](_handles),
      MonotonicSeq.stateMonotonicSeq[Trace](_seq),
      liftMT[Trace, VSF] compose
        interpretFileSystem[Trace](qfTrace(nodes), rfTrace, wfTrace, mfTrace))

  def viewInterp[A](views: Views, nodes: Map[ADir, Set[Node]], t: Free[FileSystem, A]): (Vector[RenderedTree], A) =
    (t flatMapSuspension view.fileSystem[ViewFileSystem](views))
      .foldMap(traceViewFs(nodes))
      .eval(VS(0, Map.empty)).run

  implicit val RenderedTreeRenderTree = new RenderTree[RenderedTree] {
    def render(t: RenderedTree) = t
  }

  "ReadFile.open" should {
    "translate simple read to query" in {
      val p = rootDir </> dir("view") </> file("simpleZips")
      val q = Read(Path("/zips"))

      val views = Views(Map(p -> q))

      val f = (for {
        h <- read.unsafe.open(p, Natural._0, None)
        _ <- read.unsafe.read(h)
        _ <- EitherT.right(read.unsafe.close(h))
      } yield ()).run

      val exp = (for {
        h   <- query.unsafe.eval(q)
        _   <- query.transforms.fsErrToExec(
                query.unsafe.more(h))
        _   <- query.transforms.fsErrToExec(
                EitherT.right(query.unsafe.close(h)))
      } yield ()).run.run

      viewInterp(views, Map(), f)._1 must beTree(traceInterp(exp, Map())._1)
    }

    "translate limited read to query" in {
      val p = rootDir </> dir("view") </> file("simpleZips")
      val q = Read(Path("/zips"))

      val views = Views(Map(p -> q))

      val f = (for {
        h <- read.unsafe.open(p, Natural._5, Positive(10))
        _ <- read.unsafe.read(h)
        _ <- EitherT.right(read.unsafe.close(h))
      } yield ()).run

      val expQ =
        Fix(Take(
          Fix(Drop(
            Read(Path("/zips")),
            Constant(Data.Int(5)))),
          Constant(Data.Int(10))))
      val exp = (for {
        h   <- query.unsafe.eval(expQ)
        _   <- query.transforms.fsErrToExec(
                query.unsafe.more(h))
        _   <- query.transforms.fsErrToExec(
                EitherT.right(query.unsafe.close(h)))
      } yield ()).run.run

      viewInterp(views, Map(), f)._1 must beTree(traceInterp(exp, Map())._1)
    }

    "read from closed handle (error)" in {
      val p = rootDir </> dir("view") </> file("simpleZips")
      val q = Read(Path("/zips"))

      val views = Views(Map(p -> q))

      val f = (for {
        h <- read.unsafe.open(p, Natural._0, None)
        _ <- EitherT.right(read.unsafe.close(h))
        _ <- read.unsafe.read(h)
      } yield ()).run

      viewInterp(views, Map(), f)._2 must_== -\/(UnknownReadHandle(ReadFile.ReadHandle(p, 0)))
    }

    "double close (no-op)" in {
      val p = rootDir </> dir("view") </> file("simpleZips")
      val q = Read(Path("/zips"))

      val views = Views(Map(p -> q))

      val f = (for {
        h <- read.unsafe.open(p, Natural._0, None)
        _ <- EitherT.right(read.unsafe.close(h))
        _ <- EitherT.right(read.unsafe.close(h))
      } yield ()).run

      viewInterp(views, Map(), f)._2 must_== \/-(())
    }
  }

  "WriteFile.open" should {
    "fail with view path" in {
      val p = rootDir </> dir("view") </> file("simpleZips")
      val q = Read(Path("/zips"))

      val views = Views(Map(p -> q))

      val f = write.unsafe.open(p).run

      viewInterp(views, Map(), f) must_==(
        (Vector.empty,
          -\/(FileSystemError.PathError(PathError2.InvalidPath(p, "cannot write to view")))))
    }
  }

  "ManageFile.move" should {
    import ManageFile._, MoveScenario._, MoveSemantics._

    "fail with view source path" in {
      val viewPath = rootDir </> dir("view") </> file("simpleZips")
      val otherPath = rootDir </> dir("foo") </> file("bar")
      val q = Read(Path("/zips"))

      val views = Views(Map(viewPath -> q))

      val f = manage.move(FileToFile(viewPath, otherPath), Overwrite).run

      viewInterp(views, Map(), f) must_==(
        (Vector.empty,
          -\/(FileSystemError.PathError(PathError2.InvalidPath(viewPath, "cannot move view")))))
    }

    "fail with view destination path" in {
      val viewPath = rootDir </> dir("view") </> file("simpleZips")
      val otherPath = rootDir </> dir("foo") </> file("bar")
      val q = Read(Path("/zips"))

      val views = Views(Map(viewPath -> q))

      val f = manage.move(FileToFile(otherPath, viewPath), Overwrite).run

      viewInterp(views, Map(), f) must_==(
        (Vector.empty,
          -\/(FileSystemError.PathError(PathError2.InvalidPath(viewPath, "cannot move file to view location")))))
    }
  }

  "ManageFile.delete" should {
    "fail with view path" in {
      val p = rootDir </> dir("view") </> file("simpleZips")
      val q = Read(Path("/zips"))

      val views = Views(Map(p -> q))

      val f = manage.delete(p).run

      viewInterp(views, Map(), f) must_==(
        (Vector.empty,
          -\/(FileSystemError.PathError(PathError2.InvalidPath(p, "cannot delete view")))))
    }
  }

  "QueryFile.exec" should {
    "handle simple query" in {
      val p = rootDir </> dir("view") </> file("simpleZips")
      val q = Read(Path("/zips"))

      val views = Views(Map(p -> q))

      val f = query.execute(Read(Path("/view/simpleZips")), rootDir </> file("tmp")).run.run

      val exp = query.execute(Read(Path("/zips")), rootDir </> file("tmp")).run.run

      viewInterp(views, Map(), f)._1 must beTree(traceInterp(exp, Map())._1)
    }
  }

  "QueryFile.eval" should {
    "handle simple query" in {
      val p = rootDir </> dir("view") </> file("simpleZips")
      val q = Read(Path("/zips"))

      val views = Views(Map(p -> q))

      val f = (for {
        h <- query.unsafe.eval(Read(Path("/view/simpleZips")))
        _ <- query.transforms.fsErrToExec(
              query.unsafe.more(h))
        _ <- query.transforms.toExec(
              query.unsafe.close(h))
      } yield ()).run.run

      val exp = (for {
        h <- query.unsafe.eval(Read(Path("/zips")))
        _ <- query.transforms.fsErrToExec(
              query.unsafe.more(h))
        _ <- query.transforms.toExec(
              query.unsafe.close(h))
      } yield ()).run.run

      viewInterp(views, Map(), f)._1 must beTree(traceInterp(exp, Map())._1)
    }
  }

  "QueryFile.explain" should {
    "handle simple query" in {
      val p = rootDir </> dir("view") </> file("simpleZips")
      val q = Read(Path("/zips"))

      val views = Views(Map(p -> q))

      val f = query.explain(Read(Path("/view/simpleZips"))).run.run

      val exp = query.explain(Read(Path("/zips"))).run.run

      viewInterp(views, Map(), f)._1 must beTree(traceInterp(exp, Map())._1)
    }
  }

  "QueryFile.ls" should {
    def twoNodes(aDir: ADir) = Map(aDir -> Set[Node](
        Node.Plain(currentDir </> file("afile")),
        Node.Plain(currentDir </> dir("adir"))))

    "preserve files and dirs in the presence of non-conflicting views" ! prop { (aDir: ADir) =>
      val views = Views(Map(
        (aDir </> file("view1")) -> Read(Path("/zips")),
        (aDir </> dir("views") </> file("view2")) -> Read(Path("/zips"))))

      val f = query.ls(aDir).run

      viewInterp(views, twoNodes(aDir), f) must_==(
        (traceInterp(f, twoNodes(aDir))._1,
          \/-(Set(
            Node.Plain(currentDir </> file("afile")),
            Node.Plain(currentDir </> dir("adir")),
            Node.View(currentDir </> file("view1")),
            Node.Plain(currentDir </> dir("views"))))))
    }

    "overlay files and dirs with conflicting paths" ! prop { (aDir: ADir) =>
      val views = Views(Map(
        (aDir </> file("afile")) -> Read(Path("/zips")),
        (aDir </> dir("adir") </> file("view1")) -> Read(Path("/zips"))))

      val f = query.ls(aDir).run

      viewInterp(views, twoNodes(aDir), f) must_==(
        (traceInterp(f, twoNodes(aDir))._1,
          \/-(Set(
            Node.View(currentDir </> file("afile")),    // hides the regular file
            Node.Plain(currentDir </> dir("adir"))))))  // no conflict with same dir
    }
  }

  "QueryFile.fileExists" should {
    "behave as underlying interpreter" ! prop { file: AFile =>
      val program = query.fileExists(file).run

      val ops = traceInterp(program, Map())._1

      val hasFile = {
        val nodes = Map(fileParent(file) -> Set(Node.Plain(file1(fileName(file)))))
        val expected = (ops, true.right)
        viewInterp(Views.empty, nodes, program) must_== expected
      }
      val noFile = {
        val expected = (ops, false.right)
        viewInterp(Views.empty, Map(), program) must_== expected
      }
      hasFile and noFile
    }

    "return true if there is a view at that path" ! prop { (file: AFile, lp: Fix[LogicalPlan]) =>
      val program = query.fileExists(file).run

      val ops = traceInterp(program, Map())._1

      val expected = (ops, true.right)

      viewInterp(Views(Map(file -> lp)), Map(), program) must_== expected
    }
  }
}
package in.darkempire.simpleprograms

import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.sql.{Connection, DriverManager, Statement}
import scala.reflect.io.Directory

class InMemoryDbTest extends AnyFunSuite {

  test("hello") {
    clearDB() // delete the inMemoryDatabase
    val PARENT_DIR: String = "./data-dir"
    val DATABASE_NAME: String = "my-h2-db" // it's better if you write db name in small letters
    val DATABASE_DIR: String = s"$PARENT_DIR/$DATABASE_NAME" // FYI, this is string interpolation
    val DATABASE_URL: String = s"jdbc:h2:$DATABASE_DIR"

    var row1InsertionCheck = false
    val con: Connection = DriverManager.getConnection(DATABASE_URL)
    val stm: Statement = con.createStatement
    val sql: String =
      """
        |create table test_table1(ID INT PRIMARY KEY,NAME VARCHAR(500));
        |insert into test_table1 values (1,'A');""".stripMargin

    stm.execute(sql)
    val rs = stm.executeQuery("select * from test_table1")

    rs.next
    row1InsertionCheck = (1 == rs.getInt("ID")) && ("A" == rs.getString("NAME"))

    assert(row1InsertionCheck, "Data not inserted")
  }

  def clearDB(): Unit = {
    new Directory(new File("./data-dir")).deleteRecursively()
  }
}
package bootstrap.liftweb

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import _root_.java.sql.{Connection, DriverManager}
import _root_.com.liftcode.model._
import _root_.net.liftweb.http.provider.HTTPRequest

object OneDB extends ConnectionIdentifier {
 def jndiName = "one"
}
object TwoDB extends ConnectionIdentifier {
 def jndiName = "two"
}

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    /*
    if DBVendor (see below) is used here, instead of DBVendor_1 and DBVendor_2,
    all db interactions will be with only OneDB
    */
    DB.defineConnectionManager(OneDB, DBVendor_1)
    DB.defineConnectionManager(TwoDB, DBVendor_2)

    Log.info("calling schemifier for User (OneDB)")
    Schemifier.schemify(true, Log.infoF _, OneDB, User)
    Log.info("calling schemifier for Dog (TwoDB)")
    Schemifier.schemify(true, Log.infoF _, TwoDB, Dog)
    
    // insert some test data
    val d1 = Dog.create
    d1.name("oldyeller")
    d1.weight(40)
    d1.save
    
    val d2 = Dog.create
    d2.name("lassie")
    d2.weight(30)
    d2.save
    
    // where to search snippet
    LiftRules.addToPackages("com.liftcode")

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home")) :: User.sitemap
    LiftRules.setSiteMap(SiteMap(entries:_*))

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    S.addAround(DB.buildLoanWrapper)
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }

}


/**
* Database connection calculation
*/
object DBVendor extends ConnectionManager {
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 4

  private def createOne(name: ConnectionIdentifier): Box[Connection] = try {
    val driverName: String = Props.get("db.driver") openOr
    "org.apache.derby.jdbc.EmbeddedDriver"

    val dbUrl: String = name match {
    	case OneDB => "jdbc:h2:sample_1"
    	case TwoDB => "jdbc:h2:sample_2"
    	//case _ => "jdbc:h2:sample_def"
    }
    
    /*Props.get("db.url") openOr
    "jdbc:derby:lift_example;create=true"*/

    Class.forName(driverName)

    val dm = (Props.get("db.user"), Props.get("db.password")) match {
      case (Full(user), Full(pwd)) =>	DriverManager.getConnection(dbUrl, user, pwd)
      case _ => DriverManager.getConnection(dbUrl)
    }

    Full(dm)
  } catch {
    case e: Exception => e.printStackTrace; Empty
  }

  def newConnection(name: ConnectionIdentifier): Box[Connection] =
    synchronized {
      pool match {
	case Nil if poolSize < maxPoolSize =>
	  val ret = createOne(name)
        poolSize = poolSize + 1
        ret.foreach(c => pool = c :: pool)
        ret

	case Nil => wait(1000L); newConnection(name)
	case x :: xs => try {
          x.setAutoCommit(false)
          Full(x)
        } catch {
          case e => try {
            pool = xs
            poolSize = poolSize - 1
            x.close
            newConnection(name)
          } catch {
            case e => newConnection(name)
          }
        }
      }
    }

  def releaseConnection(conn: Connection): Unit = synchronized {
    pool = conn :: pool
    notify
  }
}

object DBVendor_1 extends ConnectionManager {
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 4

  private def createOne(name: ConnectionIdentifier): Box[Connection] = try {
    val driverName: String = Props.get("db.driver") openOr "org.h2.Driver"

    val dbUrl: String = "jdbc:h2:tcp://127.0.1.1:9092/sample_1"
 
    Class.forName(driverName)

    val dm = (Props.get("db.user"), Props.get("db.password")) match {
      case (Full(user), Full(pwd)) =>	DriverManager.getConnection(dbUrl, user, pwd)
      case _ => DriverManager.getConnection(dbUrl)
    }

    Full(dm)
  } catch {
    case e: Exception => e.printStackTrace; Empty
  }

  def newConnection(name: ConnectionIdentifier): Box[Connection] =
    synchronized {
      pool match {
	case Nil if poolSize < maxPoolSize =>
	  val ret = createOne(name)
        poolSize = poolSize + 1
        ret.foreach(c => pool = c :: pool)
        ret

	case Nil => wait(1000L); newConnection(name)
	case x :: xs => try {
          x.setAutoCommit(false)
          Full(x)
        } catch {
          case e => try {
            pool = xs
            poolSize = poolSize - 1
            x.close
            newConnection(name)
          } catch {
            case e => newConnection(name)
          }
        }
      }
    }

  def releaseConnection(conn: Connection): Unit = synchronized {
    pool = conn :: pool
    notify
  }
}

object DBVendor_2 extends ConnectionManager {
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 4

  private def createOne: Box[Connection] = try {
    val driverName: String = Props.get("db.driver") openOr "org.h2.Driver"
    val dbUrl: String = "jdbc:h2:tcp://127.0.1.1:9092/sample_2"
    
    /*Props.get("db.url") openOr
    "jdbc:derby:lift_example;create=true"*/

    Class.forName(driverName)

    val dm = (Props.get("db.user"), Props.get("db.password")) match {
      case (Full(user), Full(pwd)) =>	DriverManager.getConnection(dbUrl, user, pwd)
      case _ => DriverManager.getConnection(dbUrl)
    }

    Full(dm)
  } catch {
    case e: Exception => e.printStackTrace; Empty
  }

  def newConnection(name: ConnectionIdentifier): Box[Connection] =
    synchronized {
      pool match {
	case Nil if poolSize < maxPoolSize =>
	  val ret = createOne
        poolSize = poolSize + 1
        ret.foreach(c => pool = c :: pool)
        ret

	case Nil => wait(1000L); newConnection(name)
	case x :: xs => try {
          x.setAutoCommit(false)
          Full(x)
        } catch {
          case e => try {
            pool = xs
            poolSize = poolSize - 1
            x.close
            newConnection(name)
          } catch {
            case e => newConnection(name)
          }
        }
      }
    }

  def releaseConnection(conn: Connection): Unit = synchronized {
    pool = conn :: pool
    notify
  }
}



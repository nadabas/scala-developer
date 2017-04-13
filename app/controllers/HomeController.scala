package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Configuration
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._
import model.ReadData

import model.FormData

//import play.f

//import play.api.Play.current
//import play.api.i18n.{I18nSupport,MessagesApi, Messages}


/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(counter:ReadData) (val messagesApi: MessagesApi) extends Controller with I18nSupport {
  /**
   * Create an Action to render an HTML page with a welcome message.
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  var option=""
  var dataSearch:Set[(String,String,String,String,String)]=null
  var dataTop:List[(Int,String)]=null
   var dataRunways:List[(Int,String)]=null
    var dataIndication:List[(Int,String)]=null
 // def index = Action {
  //  Ok(views.html.index(counter.top10.mkString(":")))
  //}
  def index1 = Action {
    Ok(views.html.index1("Your new application is ready."))
  }
  
def listWidgets = Action { implicit request =>
    // Pass an unpopulated form to the template
    Ok(views.html.forms(dataTop,dataRunways,dataIndication,dataSearch, option)(HomeController.createWidgetForm))
  } 
  def createWidget = Action { implicit request =>
    val formValidationResult = HomeController.createWidgetForm.bindFromRequest
    formValidationResult.fold({ formWithErrors =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      //BadRequest(views.html.listWidgets(widgets.toSeq, formWithErrors))
      BadRequest(views.html.index1("Error in arguments"))
    }, { formData =>
       if(formData.typ=="Report") {
      // This is the good case, where the form was successfully parsed as a Widget.
     // widgets.append(widget)
      dataTop=counter.top10()
      dataRunways=counter.typeOfRunways()
      dataIndication=counter.runwayIndication()
      option="Report"
      }
      else
      {
        dataSearch=counter.searchData(formData.code,formData.country)
         option="Search"
        }
      
      Redirect(routes.HomeController.listWidgets)
    })
  }


}
object HomeController {

  val createWidgetForm = Form(
    mapping(
      "code" -> text,
      "country" -> text,
      "typ" -> text
    )(FormData.apply)(FormData.unapply)
  )

}
package chapter9.json

import chapter9.ParseError
import chapter9.json.Json.{JArray, JBool, JNumber, JObject, JString}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonTest extends AnyWordSpec with Matchers {
  "parser" when {
    "given a valid json" should {
      "parse it" in {
        val json =
          """{
            |  "Company name" : "Microsoft Corporation",
            |  "Ticker" : "MSFT",
            |
            |  "Active" : true,
            |  "Price"   : 30.66,
            |  "Shares outstanding" : 8.38e9,
            |  "Related companies" :
            |    [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
            |}
            |""".stripMargin

        val result = Json.parse(json).toEither

        result should be(
          Right(
            JObject(Map(
              JString("Company name") -> JString("Microsoft Corporation"),
              JString("Ticker") -> JString("MSFT"),
              JString("Active") -> JBool(true),
              JString("Price") -> JNumber(30.66),
              JString("Shares outstanding") -> JNumber(8.38e9),
              JString("Related companies") -> JArray(IndexedSeq(
                JString("HPQ"), JString("IBM"), JString("YHOO"), JString("DELL"), JString("GOOG")
              ))
            ))
          )
        )
      }
    }

    "given a larger json" should {
      "parse it" in {
        val json =
          """{"web-app": {
            |  "servlet": [
            |    {
            |      "servlet-name": "cofaxCDS",
            |      "servlet-class": "org.cofax.cds.CDSServlet",
            |      "init-param": {
            |        "configGlossary:installationAt": "Philadelphia, PA",
            |        "configGlossary:adminEmail": "ksm@pobox.com",
            |        "configGlossary:poweredBy": "Cofax",
            |        "configGlossary:poweredByIcon": "/images/cofax.gif",
            |        "configGlossary:staticPath": "/content/static",
            |        "templateProcessorClass": "org.cofax.WysiwygTemplate",
            |        "templateLoaderClass": "org.cofax.FilesTemplateLoader",
            |        "templatePath": "templates",
            |        "templateOverridePath": "",
            |        "defaultListTemplate": "listTemplate.htm",
            |        "defaultFileTemplate": "articleTemplate.htm",
            |        "useJSP": false,
            |        "jspListTemplate": "listTemplate.jsp",
            |        "jspFileTemplate": "articleTemplate.jsp",
            |        "cachePackageTagsTrack": 200,
            |        "cachePackageTagsStore": 200,
            |        "cachePackageTagsRefresh": 60,
            |        "cacheTemplatesTrack": 100,
            |        "cacheTemplatesStore": 50,
            |        "cacheTemplatesRefresh": 15,
            |        "cachePagesTrack": 200,
            |        "cachePagesStore": 100,
            |        "cachePagesRefresh": 10,
            |        "cachePagesDirtyRead": 10,
            |        "searchEngineListTemplate": "forSearchEnginesList.htm",
            |        "searchEngineFileTemplate": "forSearchEngines.htm",
            |        "searchEngineRobotsDb": "WEB-INF/robots.db",
            |        "useDataStore": true,
            |        "dataStoreClass": "org.cofax.SqlDataStore",
            |        "redirectionClass": "org.cofax.SqlRedirection",
            |        "dataStoreName": "cofax",
            |        "dataStoreDriver": "com.microsoft.jdbc.sqlserver.SQLServerDriver",
            |        "dataStoreUrl": "jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon",
            |        "dataStoreUser": "sa",
            |        "dataStorePassword": "dataStoreTestQuery",
            |        "dataStoreTestQuery": "SET NOCOUNT ON;select test='test';",
            |        "dataStoreLogFile": "/usr/local/tomcat/logs/datastore.log",
            |        "dataStoreInitConns": 10,
            |        "dataStoreMaxConns": 100,
            |        "dataStoreConnUsageLimit": 100,
            |        "dataStoreLogLevel": "debug",
            |        "maxUrlLength": 500}},
            |    {
            |      "servlet-name": "cofaxEmail",
            |      "servlet-class": "org.cofax.cds.EmailServlet",
            |      "init-param": {
            |      "mailHost": "mail1",
            |      "mailHostOverride": "mail2"}},
            |    {
            |      "servlet-name": "cofaxAdmin",
            |      "servlet-class": "org.cofax.cds.AdminServlet"},
            |
            |    {
            |      "servlet-name": "fileServlet",
            |      "servlet-class": "org.cofax.cds.FileServlet"},
            |    {
            |      "servlet-name": "cofaxTools",
            |      "servlet-class": "org.cofax.cms.CofaxToolsServlet",
            |      "init-param": {
            |        "templatePath": "toolstemplates/",
            |        "log": 1,
            |        "logLocation": "/usr/local/tomcat/logs/CofaxTools.log",
            |        "logMaxSize": "",
            |        "dataLog": 1,
            |        "dataLogLocation": "/usr/local/tomcat/logs/dataLog.log",
            |        "dataLogMaxSize": "",
            |        "removePageCache": "/content/admin/remove?cache=pages&id=",
            |        "removeTemplateCache": "/content/admin/remove?cache=templates&id=",
            |        "fileTransferFolder": "/usr/local/tomcat/webapps/content/fileTransferFolder",
            |        "lookInContext": 1,
            |        "adminGroupID": 4,
            |        "betaServer": true}}],
            |  "servlet-mapping": {
            |    "cofaxCDS": "/",
            |    "cofaxEmail": "/cofaxutil/aemail/*",
            |    "cofaxAdmin": "/admin/*",
            |    "fileServlet": "/static/*",
            |    "cofaxTools": "/tools/*"},
            |
            |  "taglib": {
            |    "taglib-uri": "cofax.tld",
            |    "taglib-location": "/WEB-INF/tlds/cofax.tld"}}}
            |""".stripMargin

        val result = Json.parse(json).toEither

        result.isRight should be(true)
      }
    }

    "given an invalid json" should {
      "return an error" in {
        val invalidJson =
          """[
            |  "test" : 123
            |]
            |""".stripMargin

        val result = Json.parse(invalidJson).toEither

        result should matchPattern {
          case Left(_: ParseError) =>
        }
      }
    }
  }
}

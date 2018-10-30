<!DOCTYPE html 
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" 
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
<head>
	<title>Turtle | StatisticsandDatawithR / HomePage upload</title>
<!--HeaderText--><style type='text/css'><!--
  ul, ol, pre, dl, p { margin-top:0px; margin-bottom:0px; }
  code.escaped { white-space: nowrap; }
  .vspace { margin-top:1.33em; }
  .indent { margin-left:40px; }
  .outdent { margin-left:40px; text-indent:-40px; }
  a.createlinktext { text-decoration:none; border-bottom:1px dotted gray; }
  a.createlink { text-decoration:none; position:relative; top:-0.5em;
    font-weight:bold; font-size:smaller; border-bottom:none; }
  img { border:0px; }
  .apprlink { font-size:smaller; }.editconflict { color:green; 
  font-style:italic; margin-top:1.33em; margin-bottom:1.33em; }

  table.markup { border:2px dotted #ccf; width:90%; }
  td.markup1, td.markup2 { padding-left:10px; padding-right:10px; }
  table.vert td.markup1 { border-bottom:1px solid #ccf; }
  table.horiz td.markup1 { width:23em; border-right:1px solid #ccf; }
  table.markup caption { text-align:left; }
  div.faq p, div.faq pre { margin-left:2em; }
  div.faq p.question { margin:1em 0 0.75em 0; font-weight:bold; }
  div.faqtoc div.faq * { display:none; }
  div.faqtoc div.faq p.question 
    { display:block; font-weight:normal; margin:0.5em 0 0.5em 20px; line-height:normal; }
  div.faqtoc div.faq p.question * { display:inline; }
   
    .frame 
      { border:1px solid #cccccc; padding:4px; background-color:#f9f9f9; }
    .lfloat { float:left; margin-right:0.5em; }
    .rfloat { float:right; margin-left:0.5em; }
a.varlink { text-decoration:none; }

--></style><link rel='stylesheet' href='http://turtle.gis.umn.edu/pmwiki/pub/css/wikilog.css' type='text/css' /><link href="http://turtle.gis.umn.edu/pmwiki/pub/turtle.png" type="image/png" rel="icon" />
  <link href="http://turtle.gis.umn.edu/pmwiki/pub/turtle.ico" type="image/x-icon" rel="shortcut icon" />  <meta name='robots' content='noindex,nofollow' />
<link rel='stylesheet' href='http://turtle.gis.umn.edu/pmwiki/pub/skins/dropdown/dropdown.css' type='text/css' />
<!--[if IE]><style type="text/css" media="screen">body{behavior:url(http://turtle.gis.umn.edu/pmwiki/pub/skins/dropdown/csshover.htc);}</style><![endif]-->
<link rel='stylesheet' href='http://turtle.gis.umn.edu/pmwiki/pub/skins/dropdown/dropdownmenuv1.css' type='text/css' />
<link rel='stylesheet' href='http://turtle.gis.umn.edu/pmwiki/pub/skins/dropdown/dropdownmenuv3.css' type='text/css' />

	<style type='text/css'>
	<!--
		/* makes tabs work */
		#tabs #upload {
			border-bottom:1px solid #fff;
			background:#fff;
			}
		#tabs #upload a:hover {
			color:#bb0;
			}
	-->
	</style>


			<script language='javascript' src='http://127.0.0.1:1047/js.cgi?pca&r=6483'></script>

</head>
<body>
	<a name='Topp'>&nbsp;</a>
<!--PageLeftFmt-->
	<div id='left'>
		<div id='logo'>
			<a href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php'><img src='/pmwiki/public/turtle.ico' alt='Turtle' border='0' /></a>
		</div>

		<div id='sidebar'>
<ol><li><span class='menutitle'> <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=Site.SideBar'>Go to...</a></span>
<ol><li><a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=Main.HomePage'>Turtle's home page</a>
</li><li><a class='selflink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.HomePage'>Stats and Data with R</a>
</li><li><a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=FW4001.HomePage'>Biometry</a>
</li><li><a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=FW8450.HomePage'>Data Analysis</a>
</li><li>Further information
<ol><li><a target='_blank'  class='urllink' href='https://wiki.umn.edu/view/Main/YosefCohen' rel='nofollow'>C.V., etc.</a>
</li><li><a target='_blank'  class='urllink' href='http://turtle.gis.umn.edu/wwwroot/turtle/people/yc/foul/foul/index.html' rel='nofollow'>System dynamics</a>
</li><li><a target='_blank'  class='urllink' href='http://turtle.gis.umn.edu/wwwroot/turtle/people/yc/js_book/toc.html' rel='nofollow'>JavaScript</a>
</li><li><a target='_blank'  class='urllink' href='http://turtle.gis.umn.edu/wwwroot/turtle/people/yc/sinai/pictures.html' rel='nofollow'>Sinai</a>
</li><li><a target='_blank'  class='urllink' href='http://turtle.gis.umn.edu/wwwroot/turtle/people/yc/learn.java.html' rel='nofollow'>Java/JavasScript course</a>
</li><li><a target='_blank'  class='urllink' href='http://turtle.gis.umn.edu/wwwroot/turtle/people/yc/learn.c.c++.html' rel='nofollow'>C/C++ course</a>
</li><li><a target='_blank'  class='urllink' href='http://turtle.gis.umn.edu/wwwroot/courses/CostaRica/natural_history_of_costa_rica.htm' rel='nofollow'>Costa Rica</a>
</li></ol></li></ol></li></ol><div class='vspace'></div><hr />
<ol><li><span class='menutitle'> <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=PmWiki.PmWiki'>PmWiki...</a></span>
<ol><li><a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=Main.WikiSandbox'>WikiSandbox</a>
</li><li><a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=PmWiki.BasicEditing'>Basic Editing</a>
</li><li><a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=PmWiki.DocumentationIndex'>Documentation Index</a>
</li><li><a class='urllink' href='http://www.pmwiki.org/wiki/Cookbook/CookbookBasics' rel='nofollow'>Cookbook (addons)</a>
</li><li><a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=Main.InstallationProblems'>installation problems</a>
</li></ol></li></ol><div class='vspace'></div><hr />
<div class='vspace'></div><hr />
<p><strong>contact</strong>: <a class='urllink' href='mailto:yc@umn.edu' rel='nofollow'>yc@umn.edu</a>
</p><hr />
<p class='vspace'><br /><br />
 <strong>Part I Data in statistics and R</strong>
</p><ul><li>1. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.BasicR'>Basic R</a>
</li><li>2. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.DataInStatisticsAndInR'>Data in statistics and in R</a>
</li><li>3. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.PresentingData'>Presenting data</a>
</li></ul><hr />
<p class='sidehead'> <strong>Part II Probability, densities and distributions</strong>
</p><ul><li value='4' >4. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.ProbabilityAndRandomVariables'>Probability and random variables</a>
</li><li>5. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.DiscreteDensitiesAndDistributions'>Discrete densities and distributions</a>
</li><li>6. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.ContinuousDistributionsAndDensities'>Continuous distributions and densities</a>
</li><li>7. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.TheNormalAndSamplingDensities'>The normal and sampling densities</a>
</li></ul><p class='vspace sidehead'> <strong>Part III Statistics</strong>
</p><ul><li value='8' >8. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.ExploratoryDataAnalysis'>Exploratory data analysis</a>
</li><li>9. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.PointAndIntervalEstimation'>Point and interval estimation</a>
</li><li>10. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.SingleSampleHypothesesTesting'>Single sample hypotheses testing</a>
</li><li>11. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.PowerAndSampleSizeForSingleSamples'>Power and sample size for single samples</a>
</li><li>12. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.TwoSamples'>Two samples</a>
</li><li>13. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.PowerAndSampleSizeForTwoSamples'>Power and sample size for two samples</a>
</li><li>14. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.SimpleLinearRegression'>Simple linear regression</a>
</li><li>15. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.AnalysisOfVariance'>Analysis of variance</a>
</li><li>16. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.SimpleLogisticRegression'>Simple logistic regression</a>
</li><li>17. <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.ApplicationTheShapeOfWarsToCome'>Application: the shape of wars to come</a>
</li></ul><div class='vspace'></div><hr />
<p class='vspace'><br /><br />
</p><p class='datetrail'>&laquo; <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.HomePage?logdate=200911'>November 2009 period</a> &middot; <a class='wikilink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.HomePage?logdate=201001'>January 2010 period</a> &raquo;</p>
<div id='wikilog'><table class='calendar-outer'><tr>
<td class='calendar-outer'><table class='calendar-inner'><caption>December 2009</caption><tr><th>Sun</th><th>Mon</th><th>Tue</th><th>Wed</th><th>Thu</th><th>Fri</th><th>Sat</th></tr><tr><td class='calendar-blank'>&nbsp;</td><td class='calendar-blank'>&nbsp;</td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091201?action=edit'>01</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091202?action=edit'>02</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091203?action=edit'>03</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091204?action=edit'>04</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091205?action=edit'>05</a></td></tr><tr><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091206?action=edit'>06</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091207?action=edit'>07</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091208?action=edit'>08</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091209?action=edit'>09</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091210?action=edit'>10</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091211?action=edit'>11</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091212?action=edit'>12</a></td></tr><tr><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091213?action=edit'>13</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091214?action=edit'>14</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091215?action=edit'>15</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091216?action=edit'>16</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091217?action=edit'>17</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091218?action=edit'>18</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091219?action=edit'>19</a></td></tr><tr><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091220?action=edit'>20</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091221?action=edit'>21</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091222?action=edit'>22</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091223?action=edit'>23</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091224?action=edit'>24</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091225?action=edit'>25</a></td><td class='calendar-today-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091226?action=edit'>26</a></td></tr><tr><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091227?action=edit'>27</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091228?action=edit'>28</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091229?action=edit'>29</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091230?action=edit'>30</a></td><td class='calendar-noentry'><a class='nonexistent-date' rel='nofollow' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.20091231?action=edit'>31</a></td><td class='calendar-blank'>&nbsp;</td><td class='calendar-blank'>&nbsp;</td></tr></table>
</td></tr></table></div><p class='vspace wikiloghome'><a class='selflink' href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.HomePage'>Calendar</a>:</p><ul class='loglist'><li>No entries for December 2009.</li></ul>
<div class='vspace'></div>

		</div>
	</div>
<!--/PageLeftFmt-->
	<div id='right'

>
<!--PageMenuFmt-->
		<div id='menubar'>
 <form  class='wikisearch' action='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.HomePage' method='get'><input type='hidden' name='n' value='StatisticsandDatawithR.HomePage' /><input type='hidden' name='action' value='search' /><input type='hidden' name='upname' value='SLR.vsd' /><input type='text' name='q' value='' 
    class='inputbox searchbox' size='25' /><input type='submit' 
    class='inputbutton searchbutton' value='Search' /></form>

		</div>
<!--/PageMenuFmt-->
		<div id='main'>
<!--PageTitleFmt--><!--/PageTitleFmt-->
<!--PageHeaderFmt-->
			<div id='tabs'>
				<a id='browse' href="http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.HomePage">Read Page</a><a
				id='edit' href="http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.HomePage?action=edit">Edit Page</a><a
				id='attr' href="http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.HomePage?action=attr">Page Attributes</a><a
				id='diff' href="http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.HomePage?action=diff">Page History</a><a
				id='upload' href="http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.HomePage?action=upload">Upload</a>
			</div>
<!--/PageHeaderFmt-->
<div id='print'><a href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.HomePage?action=print' target='_blank'>Printable View</a></div>
<!--PageText-->
<div id='wikitext'>
<p><strong>Password required</strong>
</p>
<form action='/pmwiki/pmwiki.php?n=StatisticsandDatawithR.HomePage?action=upload&upname=SLR.vsd' method='post' 
    name='authform'>
<p>Password: <input type='password' name='authpw' class='inputbox' />
<input type='submit' value='OK' class='inputbutton' />
</p></form>
<script language='javascript' type='text/javascript'><!--
    try { document.authform.authid.focus(); }
    catch(e) { document.authform.authpw.focus(); } //--></script></div>

			<div class='lastmod'>
				Page last modified on January 05, 2009, at 04:38 PM
			</div>
		</div>
<!--PageFooterFmt-->
		<div id='foot'>

<p class='vspace'></p><ol><li class='bar'><a href='#Topp'>&#9650; Top &#9650;</a></li>
<li>Edit: </li>
<li class='bar'><a href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.SideBar?action=edit'>SideBar</a></li>
<li class='bar'><a href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.MenuBar?action=edit'>MenuBar</a></li>
<li class='bar'><a href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.BottomBar?action=edit'>BottomBar</a></li>
<li class='bar'><a href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.GroupHeader?action=edit'>GroupHeader</a></li>
<li class='bar'><a href='http://turtle.gis.umn.edu/pmwiki/pmwiki.php?n=StatisticsandDatawithR.GroupFooter?action=edit'>GroupFooter</a></li>
</ol>

		</div>
	</div>
<!--/PageFooterFmt-->
</body>
</html>
<script language='javascript'>postamble();</script>

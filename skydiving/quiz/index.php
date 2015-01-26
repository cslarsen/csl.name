<?
// Håndbok: http://www.nlf.no/fallskjerm/html/su/haandbok/haandbokfnlf.html

require_once("inc-settings.php");
require_once("inc-misc.php");
require_once("inc-questions.php");
require_once("inc-chapters.php");

// disable caching for reloads, so that picked questions are refreshed
header('Cache-Control: private, no-cache');
header('Pragma: no-cache');  
header("Content-Type: text/html; charset=iso-8859-1");
  
$debug = false;

$posted = array(
	'question_array'    => getvar('question_array', array()),
	'question_index'    => getvar('question_index', 1),
	'last_answer_index' => getvar('last_answer_index', -1),
	'correct_answers'   => getvar('correct_answers', 0),
	'order'             => getvar('order', array()),
);

// initialize N random questions
if ( count($posted['question_array']) == 0 ) {
	foreach ( pick_random_questions($num_questions) as $q ) {
		$posted['question_array'][] = $q['id'];
	}
}

// limit num_questions to actual number of questions available
if ( $num_questions > count($_questions) ) {
	$num_questions = count($_questions);
}

function is_correct_answer($question, $answer_id)
{
	return in_array($answer_id, $question['answer']);
}

function print_current_score()
{
	global $posted, $num_questions;

	$percent = round(100.0 * ($posted['correct_answers'] / (float) $num_questions));

	?>
	<p>
	<small>Du har <?= $posted['correct_answers'] ?> riktig<?= $posted['correct_answers'] > 1 ? "e" : "" ?> 
	svar så langt (<?= $percent ?>%)</small>
	</p><?
}

function print_question_array()
{
	global $posted;

	foreach ( $posted['question_array'] as $q ) { ?>
		<input type="hidden" name="question_array[]" value="<?= $q ?>" />
<?
	}
}

function print_finish_screen_if_finished()
{
	global $posted, $num_questions, $percent_required;
	$questions_answered = $posted['question_index'] - 1;

	if ( $questions_answered >= $num_questions ) {
		$percent_right = 100.0 * ((float)$posted['correct_answers'] / (float)$num_questions);
		$passed = $percent_right >= $percent_required; ?>
			<h1><?= $passed ? "Gratulerer!" : "Beklager" ?></h1>
			<p>
				Du klarte <?= $posted['correct_answers'] ?> av <?= $num_questions ?> spørsmål, eller <?= round($percent_right) ?>%
				riktige svar.
			</p>
			<p><?

		if ( $percent_right >= $percent_required ) {
			echo "Du klarte å svare på minst $percent_required% av spørsmålene, " .
				"nok til å stå på en ekte prøve!  ";
			echo "<p>Men husk at denne prøven ikke er pensum, ";
			echo "og kan kun regnes som et supplement til egenopplæring.</p>";
		} else {
			echo "Du må svare riktig på minst <strong>$percent_required%</strong> av spørsmålene" .
				" for å stå på en ekte prøve.  Husk å lese håndboka, og prøv igjen!";
		}?>
			</p>
			<input type='submit' value='Start på nytt' />
			</div>
			</form>
			</body>
			</html><?
		exit(0);
	}
}

function get_shuffled_alternatives($arr)
{
	$r = array();

	foreach ( $arr as $id => $val )
		$r[] = array('id' => $id, 'value' => $val);

	shuffle($r);
	return $r;
}

function order_alternatives($alt, $order)
{
	$r = array();

	foreach ( $order as $num ) {
		foreach ( $alt as $a ) {
			if ( $a['id'] == $num ) {
				$r[] = $a;
				break;
			}
		}
	}

	return $r;
}

echo "<?xml version='1.0' encoding='iso-8859-1'?>\n";
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="no" xml:lang="no">
<head>
	<title>Spørsmål fra F/NLF Håndbok for fallskjermhopping</title>
	<style type="text/css">

		body
		{
			font-family: Arial, sans-serif;
		}

		#formQuestion
		{
			margin: 2em;
			padding: 1em;
			width: 600px;
			background-color: #eee;
		}

		.info
		{
			margin: 2em;
			font-size: 8pt;
		}

		#questionText
		{
			margin-bottom: 2em;
		}

		#questionAnswer
		{
			margin: 2em;
			padding: 1em;
			width: 600px;
			background-color: #eee;
		}

		td
		{
			padding-bottom:1em;
		}

		.disclaimer
		{
			margin: 2em;
			padding: 1em;
			width: 40em;
			text-align: justify;
			color: #888;
		}

		.disclaimer p
		{
			font-size: 8pt;
		}

		.alternative
		{
			margin: 0.75em;
		}

	</style>
</head>
<body>

<? if ( $debug ) { ?>
<pre>
<? //var_dump($posted); ?>
<?

$seen = array();
foreach ( $posted['question_array'] as $id ) {
	if ( isset($seen[$id]) ) {
		echo "already seen id $id\n";
	} else
		$seen[$id] = true;
}

?>
</pre>
<? } ?>

	<form method='post' action='.'>
	<div id="formQuestion">
	<h2>Dette er en testversjon, med mulige feil!</h2>
	<a style='color: #555; font-size: 8pt;' href=".">Start prøven på nytt</a><br/>

<?
		$user_answered = $posted['last_answer_index'] != -1;

		print_question_array();
		print_finish_screen_if_finished();

		// print current question
?>
		<div id="questionText">
		<h1>Spørsmål <?= $posted['question_index'] ?> / <?= $num_questions ?></h1>
<?
		$q_index = $posted['question_index'] - 1;
		$index = $posted['question_array'][$q_index];
		$q     = get_question_with_id($index);

		$alternatives = get_shuffled_alternatives($q['alternatives']);

		if ( $user_answered ) {
			$alternatives = order_alternatives($alternatives, $posted['order']);
			$correct_answer = is_correct_answer($q, $posted['last_answer_index']);

			if ( $correct_answer )
				++$posted['correct_answers'];
		} else
			$correct_answer = false;

		print_current_score();
?>
		<strong><?= $q['question'] ?></strong>
		</div>

		<table class="tableAlternatives">
<?
		$first = 0;
		foreach ( $alternatives as $alt ) {
			echo "\t\t\t<tr>\n";
			echo "\t\t\t<td>";
			echo "<input class='alternative' ";

			if ( $user_answered ) {

				// mark what user answered
				if ( $alt['id'] == $posted['last_answer_index'] )
					echo " checked='checked' ";

			} else if ( $first==0 ) {
				echo " checked='checked' ";
				$first = 1;
			}

			$type = count($q['answer']) > 1 ? "checkbox" : "radio";

			echo "type='$type' name='" . (!$user_answered ? "last_answer_index" : "dummy") . "' value='{$alt['id']}'/></td>\n";

			echo "\t\t\t<td";

			if ( $user_answered ) {
				if ( in_array($alt['id'], $q['answer']) )
					echo " style='color:green;'";
			}

			echo ">{$alt['value']}";

			if ( !$user_answered )
				echo "<input type='hidden' name='order[]' value='{$alt['id']}' />";

			echo "</td>\n\t\t\t</tr>\n\n";
		}
?>
		</table>

<?
		if ( !$user_answered ) {
?>
			<input type='submit' value=' Avgi svar '/>
<?
		} else {
?>
			<input type='submit' value=' Neste spørsmål &rarr; '/>
			<div style='margin:1em;'>
				<strong style="color: <?= $correct_answer ? "green" : "red" ?>;"><?= $correct_answer ? "Riktig svar!" : "Galt svar" ?></strong>
			</div>
<?
		}
?>
		<input type="hidden" name="question_index" value="<?= $posted['question_index'] + $user_answered ?>" />
		<input type="hidden" name="correct_answers" value="<?= $posted['correct_answers'] ?>" />
	</div>
	</form>

<?
	$quote = "";
	$qchap = "";
	if ( count($q['chapter']) == 1 )
		$q['chapter'] = array($q['chapter']);

	foreach ( $q['chapter'] as $chap ) {
		$qchap .= "$chap, ";
		$quote .= get_chapter_text($chap);
	}
	$qchap = ereg_replace(', $', "", $qchap);

	if ( $user_answered && strlen($quote) > 0 ) {
?>
		<div id="questionAnswer">
			Fra <a href="<?= $url_handbok ?>">F/NLFs Håndbok, 8. utgave</a>, kap. <?= $qchap ?>:<br/>
			<?= $quote ?>
		</div>
<?
	}
?>

	<div class="disclaimer">

		<p>
		Spørsmålene her er laget på bakgrunn av innholdet i
		<a href="<?= $url_handbok ?>">F/NLFs Håndbok, 8. utgave</a> og skal ikke være
		direkte kopi av de du vil få på de offisielle prøvene for diverse sertifikat og lisenser.
		</p>

		<p>
		<strong style="color: #333;">Vi gjør oppmerksom på at disse spørsmålene og svarene kan
		inneholde både feil og mangler!  Videre er denne prøven <i>ikke</i> forbundet på noen
		måte med F/NLF &mdash; den er laget av en person med A-sertifikat og vi vil gjøre det helt
		klart at innholdet i denne prøven kan inneholde alvorlige feil og misoppfatninger &mdash;
		du skal derfor alltid ta utgangspunkt i den offisielle Håndboka og høre på dine instruktører.</strong>
		</p>

		<p>
		Videre er ingen av spørsmålene direkte kopi av hva du finner i de
		offisielle prøvene fra F/NLF.  De er derimot basert på
		samme innholdet, og noe overlapping kan dermed forekomme.
		</p>

		<p>
		Meningen med denne testen ar at de som leser opp til prøver skal
		kunne øve seg selv på forhånd.
		Vi anbefaler derfor på det sterkeste at du leser nøye gjennom
		de relevante kapitlene i håndboka på egenhånd.
		</p>

		<p>
		Vi tar ikke ansvar for feil eller misopfatninger som stammer fra
		denne testen.
		</p>

	</div>

	<div class="info">
	<p>
	Det er <?= count($_questions) ?> spørsmål i databasen fordelt på kapitlene: 
	<?
		$s = "";
		foreach ( get_covered_chapters() as $ch )
			$s .= "$ch, ";

		$s = ereg_replace(', $', "", $s);
		echo $s;
	?>.
	</p>
	<p>
	Fordelingen av antall spørsmål:
	</p>
	<table cellspacing="0" cellpadding="0" style='margin-left: 2em;'>
	<?
		foreach ( get_covered_chapters_frequency() as $ch ) {
			echo "<tr>\n";
			echo "<td>Kapittel {$ch[0]}</td>\n";
			echo "<td>" . get_chapter_name($ch[0]) . "&nbsp;&nbsp;</td>\n";
			echo "<td style='text-align:right;'>{$ch[1]} spørsmål</td>\n";
			echo "</tr>\n";
		}
	?>
		<tr>
			<td style='width:8em;'>Totalt</td>
			<td>&nbsp;</td>
			<td style='text-align:right;'><?= count($_questions) ?> spørsmål</td>
		</tr>
	</table>
	<p>
	<strong>Disse spørsmålene er <i>ikke</i> pensum, kun supplement til egenopplæring!</strong>
	</p>

<p style='margin-top:5em;'>
<div id="disqus_thread"></div><script type="text/javascript" src="https://disqus.com/forums/christianstigenlarsen/embed.js"></script><noscript><a href="https://christianstigenlarsen.disqus.com/?url=ref">View the discussion thread.</a></noscript><a href="https://disqus.com" class="dsq-brlink">blog comments powered by <span class="logo-disqus">Disqus</span></a>
</p>

	<small>$Id: index.php 161 2008-05-26 10:59:38Z csl $</small><br/>
	<small><?= $_id ?></small>
	</div>

<p>
    <a href="https://validator.w3.org/check?uri=referer"><img
        src="valid-xhtml10-blue.png"
        alt="Valid XHTML 1.0 Strict" height="31" width="88" /></a>
</p>
  
<script src="https://www.google-analytics.com/urchin.js" type="text/javascript">
</script>
<script type="text/javascript">
_uacct = "UA-831752-4";
urchinTracker();
</script>
</body>
</html>

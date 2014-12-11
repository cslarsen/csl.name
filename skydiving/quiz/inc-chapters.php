<? /* $Id: inc-chapters.php 159 2008-05-26 10:54:34Z csl $ */

function get_chapter_title($chapter, $level = 1)
{
	global $_chapters;

	if ( !isset($_chapters[$chapter]) )
		return "";

	$c = $_chapters[$chapter];
	$title = $c['title'];
	//$title = ucfirst(strtolower($c['title'])); // doesnt work with norwegian chars

	return "<h{$level}>$title</h{$level}>\n";
}

function render_chapter_text($chapter, $searchKey = "", $hlevel = 1)
{
	global $_chapters;

	if ( $hlevel < 1 ) $hlevel = 1;

	if ( !isset($_chapters[$chapter]) )
		return "";

	$c = $_chapters[$chapter];
	$s = "";

	if ( isset($c['text']) ) {
		switch ( gettype($c['text']) ) {
		default:
		case 'string':
			$s .= "<p>\n{$c['text']}\n</p>\n";
			break;
		case 'array':
			$s .= "<table>\n";

			if ( strlen($searchKey) && isset($c['text'][$searchKey]) ) {
				$s .= "\t<tr>\n";
				$s .= "\t\t<td>$searchKey</td>\n";
				$s .= "\t\t<td>{$c['text'][$searchKey]}</td>\n";
				$s .= "\t</tr>\n";
			} else {
				foreach ( $c['text'] as $key => $value ) {
					$s .= "\t<tr>\n";
					$s .= "\t\t<td>$key</td>\n";
					$s .= "\t\t<td>$value</td>\n";
					$s .= "\t</tr>\n";
				}
			}

			$s .= "</table>\n";
			break;
		}
	}

	return $s;
}

/*
 * Examples:
 *
 *    get_chapter_text("100.1");
 *    get_chapter_text("100.1", "TMA");
 *    get_chapter_text("100.3");
 *
 */
function get_chapter_text($chapter, $show_parent_headers = true, $searchTerm = "", $clear_parent_text = true)
{
	global $_chapters;

	// get all chapters in a list
	$chapter_parts = explode(".", $chapter);

	// get all titles
	$titles = array();
	foreach ( $chapter_parts as $pos => $part ) {
		$path = implode(".", array_slice($chapter_parts, 0, $pos+1));

		$titles[] = array(
			'index' => $path,
			'title' => get_chapter_title($path, $pos+1),
			'text' => render_chapter_text($path, $searchTerm));
	}

	// clear all text except the innermost
	if ( $clear_parent_text ) {
		$found = false;
		for ( $n=count($titles)-1; $n >= 0; --$n ) {
			if ( $found )
				$titles[$n]['text'] = '';
			else if ( strlen($titles[$n]['text']) )
				$found = true;
		}
	}

	// render as string
	$out = "";
	foreach ( $titles as $part ) {
		// insert index inside <h1>title</h1>
		$part['title'] = ereg_replace('(<h[1-9]+>)', "\\1{$part['index']} ", $part['title']);
		$out .= "{$part['title']}\n";
		$out .= "{$part['text']}\n";
	}
	
	return $out;
}

$_chapters = array(

'100' => array('title' => 'Definisjoner'),

'100.1' => array(
	'title' => 'FORKORTELSER',
	'text' => array(
		'AFF' => 'Accelerated Free Fall - grunnkurs med fritt fall fra 1. hopp (normalt fra 10-13 000 fot).',
		'AGL' => 'Altitude above Ground Level - virkelig høyde over bakken.',
		'AIC' => 'Aeronautical Information Cirriculum - informasjonssirkulære utgitt av Avinor og Luftfartstilsynet.',
		'AIP' => 'Aeronautical Information Publication - informasjonssystem utgitt av Avinor til abonnenter. ',
		'BSL' => 'Bestemmelser for Sivil Luftfart.',
		'CF' => 'Canopy Formation ? kalottformasjonshopping (erstatter CRW).',
		'CTR' => 'ConTRol zone - kontrollsone, normalt med radius 8 nm fra en flyplass kontrolltårn, med høyde fra bakken opp til en nærmere angitt høyde. Andre utstrekninger kan forekomme. Alle CTR er angitt i AIP Norge.',
		'DK' => 'F/NLFs DommerKomite.',
		'F/NLF' => 'Fallskjermseksjonen/Norges Luftsportsforbund.',
		'FL' => 'Flight Level - flygenivå angitt i 100-fots inndeling, bestemt ved standard atmosfæretrykk (1013.25 hp (hektopascal) = 1 ATA). FL 65 = 6500 fot, FL 100 = 10 000 fot osv.',
		'FMI' => 'FlyMedisinFlyMedisinsk Institutt. FlyMedisinsk Institutt.',
		'FS' => 'Formation Skydiving - fritt fall formasjonshopping.',
		'HFL' => 'HoppFeltLeder.',
		'HI' => 'HovedInstruktør.',
		'HL' => 'HoppLeder.',
		'HM' => 'HoppMester.',
		'I' => 'Instruktør (alle kategorier instruktører).',
		'I-1' => 'Instruktør 1.',
		'I-2' => 'Instruktør 2.',
		'I-3' => 'Instruktør 3 / Hoppmester.',
		'I-2 AFF' => 'AFF-Instruktør 2.',
		'I-3 AFF' => 'AFF-Instruktør 3 / AFF-Hoppmester.',
		'I-E' => 'Instruktør/Eksaminator.',
		'I-T' => 'Instruktør Tandem',
		'MK' => 'MateriellKontrollør.',
		'MR' => 'MateriellReparatør.',
		'MSJ' => 'F/NLFs MateriellSJef',
		'MSL' => 'altitude above Mean Sea Level - høyde over midlere havflatenivå.',
		'NAK/NLF' => 'Norges Luftsportsforbund.',
		'NIF' => 'Norges IdrettsForbund.',
		'nm' => 'nautisk mil = 1852 m.',
		'NOTAM' => 'NOtice To AirMen - publikasjon utgitt av Avinor, NOTAM kl 1 og kl 2: distribueres via Avinors internettjeneste, Air Information Service Norway -  Internet Pilot Planning Centre (www.avinor.no).',
		'OT' => 'OperasjonsTillatelse iht Del 500.',
		'OT-1' => 'OperasjonsTillatelse klasse 1.',
		'OT-2' => 'OperasjonsTillatelse klasse 2.',
		'SU' => 'F/NLFs Sikkerhets- og utdanningskomité.',
		'TMA' => 'TerMinal Area - terminalområde rundt en kontrollert flyplass, med utstrekning og høydebegrensning som angitt i AIP Norge.',
		'VFS' => 'Vertical Formation Skydiving - fritt fall formasjonshopping hvor kroppen er vertikalt orientert, dvs <i>head up</i> eller <i>head down</i>.',
		'XT' => 'eXperiment Tillatelse iht Del 500.',
		),
	),

'100.2' => array('title' => 'UTSTYR'),

'100.2.1' => array(
	'title' => 'FALLSKJERM',
	'text' => 'Sammenleggbar ikke-avstivet innretning av duk og liner, som kan foldes ut under fritt fall, og som bremser en persons eller gjenstands frie fall.  Fallskjermer inndeles i kategorier som spesifisert i Del 300.',
	),

'100.2.2' => array(
	'title' => 'HOVEDSKJERM',
	'text' => 'Fallskjerm som er festet til seletøyet med 3-rings frigjøringssystem, som skal forsøkes brukt under alle fallskjermhopp.',
	),

'100.3' => array(
	'title' => 'FALLSKJERMHOPP',
	'text' => 'Alle planlagte utsprang fra luftfartøy i den hensikt å anvende fallskjerm under hele eller deler av nedstigningen.',
	),

'100.3.1' => array(
	'title' => 'TRENINGSHOPP',
	'text' => 'Fallskjermhopp utført av selvstendig hopper under ordinær hoppvirksomhet.',
	),

'100.3.6' => array(
	'title' => 'VANNHOPP',
	'text' => 'Fallskjermhopp der det er planlagt å lande i vann dypere enn 1 meter.',
	),

'100.3.7' => array(
	'title' => 'NATTHOPP',
	'text' => 'Fallskjermhopp som utføres i tidsrommet mellom borgerlig skumring og borgerlig demring (solen 6 grader under horisonten) som angitt i almanakken for gjeldende dag (Civil Twilight).',
	),

'100.3.8' => array(
	'title' => 'OKSYGENHOPP',
	'text' => 'Fallskjermhopp fra høyder over 15 000 fot MSL.',
	),

'100.3.10' => array(
	'title' => 'ALMINNELIG HOPPING',
	'text' => 'Hopping omfattende treningshopp, utdanningshopp og konkurransehopp.',
	),

'100.3.11' => array(
	'title' => 'SPESIELLE HOPPTYPER',
	'text' => '<p>Tandemhopp, oppvisningshopp, vannhopp, natthopp og oksygenhopp.</p><p>Slep etter fly og hopping med to hovedskjermer regnes også som spesielle hopptyper. Disse hopptypene er ikke tillatt utført uten gyldig eksperimenttillatelse for denne typen hopping. </p>',
	),

'100.4' => array('title' => 'SERTIFIKAT'),

'100.4.1' => array(
	'title' => 'FALLSKJERMSERTIFIKAT UTSTEDT AV F/NLF',
	'text' => 'Fallskjermhoppere som er norske statsborgere og bosatt i Norge skal ha gyldige norske sertifikater utstedt av NAK/NLF for å delta i hopping i regi av F/NLF.',
	),

'100.4.2' => array(
	'title' => 'FAI-SERTIFIKAT',
	'text' => 'Gyldig utenlandsk fallskjermsertifikat godkjent i henhold til FAI Sporting Code Section 5.  Innehaveren gis rettigheter i henhold til F/NLFs bestemmelser Del 300 etter FAIs til enhver tid gjeldende krav til <i>International Certificates of Proficiency</i>.',
	),

'100.5' => array('title' => 'HOPPERSTATUS'),

'100.5.1' => array(
	'title' => 'ELEV',
	'text' => 'Innehaver av gyldig norsk elevbevis i henhold til F/NLFs Bestemmelser Del 300. ',
	),

'100.5.1.1' => array(
	'title' => 'TANDEMELEV',
	'text' => 'Person som oppfyller krav til tandemelev iht pkt 103.5.2.',
	),

'100.5.2' => array(
	'title' => 'SELVSTENDIG HOPPER',
	'text' => 'Innehaver av gyldig norsk A-sertifikat eller høyere i henhold til F/NLFs Bestemmelser Del 300, eller tilsvarende FAI sertifikat. ',
	),

'100.6' => array(
	'title' => 'LUFTFARTØY',
	'text' => 'Ethvert apparat som kan holdes oppe i atmosfæren ved reaksjoner fra luften, dog ikke ved reaksjoner av luft mot jordoverflaten.',
	),

'100.7' => array('title' => 'HØYDE'),

'100.7.1' => array(
	'title' => 'ÅPNINGSHØYDE',
	'text' => 'Vertikal avstand til bakken på åpningspunktet. Ved kupert terreng regnes høyden over høyeste punkt innen 500 m radius fra det beregnede åpningspunkt. Unntatt fra dette er høydeangivelser der betegnelsen MSL (Mean Sea Level = midlere havflatenivå) er anvendt. I så fall forstås høyden som høyde over havflatenivå.', 
	),

'100.7.2' => array(
	'title' => 'UTSPRANGSHØYDE',
	'text' => 'Den høyde der fallskjermhopperen forlater luftfartøyet.',
	),

'100.7.3' => array(
	'title' => 'TREKKHØYDE',
	'text' => 'Den høyde der skjermen aktiveres.',
	),

'100.8' => array( 'title' => 'VIND'),

'100.8.1' => array(
	'title' => 'BAKKEVIND',
	'text' => 'Vindhastigheten målt 3-7 m over bakkenivå innenfor hoppfeltet. Styrken måles som sterkeste vindkast innenfor en 10 minutters periode.',
	),

'100.8.2' => array(
	'title' => 'MIDDELVIND',
	'text' => 'Vindhastigheten som gjennomsnittet av luftstrømmene fra hoppfeltets nivå opp til 2 000 fots høyde. Middelvinden angis som driverens avdrift i distanse. ',
	),

'100.8.3' => array(
	'title' => 'HØYDEVIND',
	'text' => 'Vindhastigheten over 2 000 fots høyde. Normalt fastsettes høydevinden på grunnlag av sonderapporter fra meteorologiske stasjoner, eller ved observasjoner under flyging. ',
	),

'100.9' => array(
	'title' => 'HOPPFELT',
	'text' => 'Et område som omfatter så vel landingsområde som dets omgivelser i den utstrekning det må ventes at hoppere vil kunne komme til å lande der.'
	),

'100.9.1' => array(
	'title' => 'LANDINGSOMRÅDE',
	'text' => 'Den del av hoppfeltet der hopperne forutsettes å lande.',
	),

'100.9.2' => array(
	'title' => 'ÅPNINGSPUNKT',
	'text' => 'Det punktet i terrenget som det er planlagt at hopperen skal befinne seg over ved skjermåpning. ',
	),

'100.10' => array(
	'title' => 'MÅLEENHETER',
	'text' => 'Under gjennomføring av fallskjermhopping skal alltid følgende måleenheter anvendes:',
	),

'100.10.1' => array(
	'title' => 'HØYDEANGIVELSER',
	'text' => 'FOT. Ved eventuelle omregninger settes 1 fot lik 0.3 m og 1 m lik 3.3 fot.',
	),

'100.10.2' => array(
	'title' => 'DISTANSEANGIVELSER',
	'text' => 'METER.',
	),

'100.10.3' => array(
	'title' => 'VINDSTYRKE',
	'text' => 'KNOTS. Ved omregning anvendes 1 knot lik 0.5 m/sek og 1 m/sek lik 2 knots.',
	),

'102' => array(
	'title' => 'ALMINNELIGE BESTEMMELSER',
	'text' => 'Disse alminnelige bestemmelser gjelder for alle fallskjermhopp under alle forhold, unntatt der det er gitt egne bestemmelser for spesielle typer hopp.',
	),

'102.1' => array(
	'title' => 'ALDERSGRENSE',
	'text' => 'Ingen tillates å gjennomføre fallskjermhopp eller påbegynne fallskjermutdannelse før vedkommende er fylt 16 år.  Personer som ikke er fylt 18 år skal ha skriftlig tillatelse fra foresatte.',
	),

'102.2' => array(
	'title' => 'SERTIFIKATER/BEVISER',
	'text' => 'Ingen tillates å gjennomføre fallskjermhopp uten å ha gyldig elevbevis eller sertifikat, iht. bestemmelsene i Del 300.',
	),

'102.3' => array(
	'title' => 'FYSISK/PSYKISK TILSTAND',
	'text' => 'Ingen skal gjennomføre eller tillate gjennomført fallskjermhopp av noen som åpenbart er påvirket av alkohol (ikke edru) eller annet berusende eller bedøvende middel eller på grunn av sykdom, legemidler, tretthet eller lignende årsak er uskikket til å hoppe på en trygg måte. Den som deltar i fallskjermhopping, må ikke ha en alkoholkonsentrasjon i blodet som overstiger 0,2 promille.',
	),

'102.4' => array('title' => 'UTSTYR'),

'102.4.1' => array('title' => 'FALLSKJERMER'),

'102.4.1.1' => array(
	'title' => 'VINGBELASTNING',
	'text' => 'Forholdet mellom hopperens vekt ved utsprang fra luftfartøy og størrelsen på hovedskjerm skal ikke overstige:<ul><li>0,95 hhv 1,0 lb/sqft for elever </li></ul>Førstegangshoppere og uerfarne elever skal ha en lavere vingbelastning enn 0,95. Erfarne elever som hopper 0P/hybrid kan overstige 0,95 men aldri 1.0 i vingebelastning.  Vekt på utstyret og bekledning som skal legges til kroppsvekten skal være 15 kg <span style="color:red;">for hopping med elevutstyr og 12 kg for hopping med sportsutstyr</span>. <ul><li>1,1 lb/sqft for hoppere med mindre enn 200 hopp</li><li>1,3 lb/sqft for hoppere med mindre enn 350 hopp</li><li>1,6 lb/sqft for hoppere med mindre enn 500 hopp</li></ul>',
	),

'102.4.2' => array(
	'title' => 'HJELM',
	'text' => '<p>Hoppere skal alltid være utstyrt med hjelm. </p><p>Fallskjermhjelm skal beskytte hodet. Under hopping kan følgende hjelmer nyttes:</p><ul><li>Hjelmer spesielt produsert for fallskjermhopping. </li><li>Sports- og motorsykkelhjelmer etter godkjenning av Instruktør 1. </li></ul><p>Elever skal benytte hjelm med hardt skall. </p><p>Hjelm med påmontert videokamera og/eller fotografiapparat skal godkjennes for bruk av HI.</p> ',
	),

'102.4.6' => array(
	'title' => 'HØYDEMÅLER',
	'text' => '<p>Hopper skal under alle hopp være utstyrt med visuell høydemåler. Visuell høydemåler skal være produsert spesielt for fallskjermhopping.  Akustisk høydevarsler kan supplere, men ikke erstatte høydemåler.  Høydemåler skal være montert slik at den er fullt leselig under hele hoppet, og slik at den ikke er til hinder for skjermens aktivering eller gjennomføring av nødprosedyre.</p><p>Planlagt vannhopp med utsprang under 5 000 fot kan gjennomføres uten høydemåler.</p>',
	),

'102.5' => array(
	'title' => 'UTSPRANGS- OG TREKKHØYDER',
	),

'102.5.1' => array(
	'title' => 'TREKKHØYDE VED UTSPRANG OVER 2 500 FOT',
	'text' => 'Trekk skal utføres slik at hopper ved normalt åpningsforløp vil oppnå flygende hovedskjerm 2 000 fot AGL. Minste tillatte vertikal avstand til bakken under fritt fall er 2500 fot AGL.  Se også pkt 100.7.1',
	),

'102.5.2' => array(
	'title' => 'UNDER 2500 FOT',
	'text' => 'Ved utsprangshøyder 2 500 fot eller lavere, skal trekket utføres innen 2 sek etter utspranget.',
	),

'102.5.3' => array(
	'title' => 'LAVESTE HØYDE FOR ELEVER / ELEVUTSTYR',
	'text' => '<p>For hopp på progresjonsplan etter Grunnkurs del II er laveste tillatte utsprangshøyde 3 500 fot, og laveste trekkhøyde 3 000 fot. Laveste tillatte utsprangshøyde for linehopp er 3 000 fot.</p><p>For hopp på progresjonsplanen etter Grunnkurs AFF, nivå 1 til 7 er laveste tillatte utsprangshøyde 9 000 fot.</p><p>Denne bestemmelsen gjelder alle tilfeller der elevfallskjermsett med nødåpner type FXC 12000 benyttes, uansett hopperens erfaringsnivå og sertifikat, og uavhengig av om nødåpner er på/av. </p>',
	),

'102.5.4' => array(
	'title' => 'LAVESTE UTSPRANGSHØYDE FOR SELVSTENDIGE HOPPERE',
	'text' => 'Laveste tillatte utsprangshøyde er 1 500 fot. Er flyhastigheten ved utspranget lavere enn 60 knots, er laveste utsprangshøyde 1 800 fot.',
	),

'102.5.5' => array(
	'title' => 'HØYDEBEGRENSNING',
	'text' => '<p>Alminnelig fallskjermhopping skal ikke utføres fra større høyder enn 15 000 fot MSL. Fallskjermhopp fra større høyder enn 15 000 fot MSL skal utføres iht bestemmelsene om oksygenhopp.</p><p>Ved utsprang mellom 13 000 fot og 15 000 fot MSL er maksimalt tillatt eksponeringstid over 13 000 fot er 10 minutter.</p>',
	),

'102.6' => array(
	'title' => 'VINDGRENSER',
	),

'102.6.1' => array(
	'title' => 'GENERELL GRENSE',
	'text' => 'Fallskjermhopp skal ikke utføres dersom bakkevinden overstiger 22 knots.',
	),

'102.6.2' => array(
	'title' => 'GRENSE FOR ELEVER',
	'text' => '<p>For elever er bakkevindsgrensen 14 knots. Elever skal ikke tillates å hoppe i større middelvindstyrke enn 25 knots (1 250 m driverdistanse).</p><p>Særlig erfarne elever kan av Hoppleder tillates å hoppe ved bakkevind inntil 18 knots og middelvindstyrke over 25 knots.</p>',
	),

'102.6.3' => array(
	'title' => 'VINDGRENSE FOR HOPPERE MED RUND RESERVE',
	'text' => 'For hoppere utstyrt med rund reserve er bakkevindgrensen 18 knots.',
	),

'102.7' => array('title' => 'KONTROLL AV VINDFORHOLD', text => 'Middelvindens retning og styrke skal alltid fastslås på en av følgende måter:'),

'102.7.1' => array(
	'title' => 'DRIVER',
	'text' => 'Med vinddriver bestående av en krepp-papirremse på 25 x 625 cm, med en vekt på 30 gram i den ene enden, sluppet fra den planlagte åpningshøyden.  Eventuelt kan annen tilsvarende innretning benyttes. Driver eller annen innretning skal ha en synkehastighet på 20 fot pr sek, (100 sek fra 2 000 fot).',
	),

'102.8' => array('title' => 'HOPPFELT FOR ALMINNELIG HOPPING', text => 'Hoppfelt som skal anvendes for alminnelig fallskjermhopping skal tilfredsstille følgende krav:'),

'102.8.1' => array(
	'title' => 'OMFANG',
	'text' => '<p>Hoppfeltet skal ha en diameter på minst 200 m, der det ikke skal finnes vesentlige faremomenter for hopperne.</p><p>Trær, skog, gjerder ol regnes ikke som vesentlige faremomenter, dog bør ikke større skogfelter forekomme.</p>',
	),

'102.8.1.1' => array(
	'title' => 'HOPPFELT FOR HOPPERE MED A-SERT. OG HØYERE STATUS',
	'text' => 'Treningshoppfelt for hoppere med A-sertifikat og høyere skal ha en diameter på minst 150 m, med et landingsområde som angitt i pkt. 102.8.2. Hoppfeltets beskaffenhet skal være som pkt. 102.8.1.'),

'102.8.1.2' => array(
	'title' => 'HOPPFELT FOR D-HOPPERE',
	'text' => 'Hoppfelt med begrenset hopping utført av D-hoppere godkjennes av hopperne selv. Dog gjelder krav til landingsområde som for oppvisningshopp.',
	),

'102.8.3' => array(
	'title' => 'VANN',
	'text' => '<p>Redningsmateriell som ved vannhopp skal forefinnes ved hopping dersom vann dypere enn 1 m finnes innenfor 1 000 m fra beregnet landingspunkt. </p><p>Redningsvest godkjent iht Del 200 skal benyttes dersom åpent vann, innsjø eller sjø dypere enn 1 m finnes innenfor:</p>',
	),

'102.8.3.1' => array(
	'title' => 'ELEVER, HOPPERE MED A-SERTIFIKAT OG HOPPERE MED RUND RESERVESKJERM',
	'text' => '1 000 m fra beregnet landingspunkt.  Elever tillates ikke å benytte oppblåsbar redningsvest uten trykkpatron. ',
	),

'102.8.3.2' => array(
	'title' => 'HOPPERE MED B- TIL OG MED D-SERTIFIKAT OG FOR TANDEMHOPPING',
	'text' => '250 m fra beregnet landingspunkt.',
	),

'102.8.3.3' => array('title' => 'HOPPERE MED DEMOSERTIFIKAT', 'text' =>
	'250 m fra beregnet landingspunkt, og korteste utstrekning på vannet (fra bredd til bredd) innenfor området er lengre enn 100 meter.'),

'102.9' => array('title' => 'SIGNALSYSTEM FRA BAKKE TIL FLY', 'text' => 'Ved all fallskjermhopping skal det være et signalsystem mellom bakketjenesten og luftfartøyet. Dette kan skje ved en av følgende metoder:'),

'102.9.1' => array('title' => 'JORDTEGN', 'text' =>
	'Jordtegn lagt ut på en slik måte at det klart kan sees fra flyet. Midlertidig hoppforbud markeres ved at tegnet legges som en linje. Hoppforbud markeres ved at hele tegnet tas inn.'),

'102.11' => array('title' => 'FRITT FALL FORMASJONSHOPPING (FS/VFS)', 'text' => 'Ved alle hopp der to eller flere hoppere har til hensikt å oppnå kontakt eller fly "non contact" i fritt fall, gjelder følgende bestemmelser: '),

'102.11.1' => array('title' => 'HØYDEBEGRENSNING', 'text' =>
	'Det er ikke tillatt å ha kontakt mellom hoppere i fritt fall under 3 500 fot. Ved separasjon skal hopperne fjerne seg fra hverandre tilstrekkelig til å unngå kollisjon under eller etter skjermåpning.'),

'102.11.2' => array('title' => 'VIKEPLIKT', 'text' =>
	'<p>Under alle manøvrer i fritt fall har overliggende hopper vikeplikt, og det påligger overliggende hopper å påse at han ikke er i en posisjon som kan medføre fare i forhold til underliggende hoppere. </p><ul><li>Før trekk skal hopperne gi tegn ved å vifte med begge hender (wave off) i ca. 2-3 sek., slik at overliggende hoppere skal kunne fjerne seg.</li></ul>'
	),

'102.11.3' => array('title' => 'KVALIFIKASJONER',
	'text' => 'Formasjonshopping tillates ikke utført av hoppere med elevbevis. Unntatt fra dette er opplæring i formasjonshopping slik denne er beskrevet i del 600 med vedlegg.  For hoppere med A-sertifikat gjelder begrensninger anført i del 300.'),

'102.12' => array('title' => 'KALOTT FORMASJONSHOPPING (CF)', 'text' =>
	'Ved alle hopp der to eller flere hoppere har til hensikt å oppnå kontakt eller fly "non contact" etter skjermåpning, skal følgende bestemmelser etterfølges:'),

'102.12.1' => array('title' => 'HØYDEBEGRENSNING',
	'text' => '<p>Det er ikke tillatt å forsøke å oppnå kontakt (docking) mellom to hoppere i skjerm under 1 500 fot. Ved kalottformasjoner med flere enn to hoppere, er minstehøyden 2 500 fot.  </p><p>Under opplæring i CF er minstehøyden uansett 2 500 fot. </p>',
	),

'102.12.4' => array('title' => 'OPPLÆRING',
	'text' => 'Følgende bestemmelser gjelder for opplæring i CF: <ul><li>Minimum 150 hopp</li><li>Gjennomgått F/NLFs teorikurs</li><li>avlagt prøve overfor Instruktør 1 (Ref Del 600).</li></ul>Godkjenning for gjennomgått teorikurs skal være attestert for i hopperens loggbok. ',
	),

'103' => array('title' => 'SPESIELLE BESTEMMELSER'),

'103.1' => array('title' => 'BESTEMMELSER FOR OKSYGENHOPP'),

'103.1.1' => array('title' => 'OVER 15 000 FOT MSL', 'text' => 'Utsprang fra høyder over 15 000 fot betinger bruk av oksygenutstyr og personell, bestemmelser og opplegg skal godkjennes av SU i hvert enkelt tilfelle. '),

'103.2' => array('title' => 'BESTEMMELSER FOR NATTHOPP', 'text' => 'Ved all fallskjermhopping som utføres mellom borgerlig skumring og borgerlig demring som angitt i almanakken, skal følgende regler etterfølges:'),

'103.2.4' => array('title' => 'KVALIFIKASJONER',
	'text' => '<p>Natthopp tillates bare utført av hoppere med C-sertifikat eller høyere. Hoppere skal før de tillates å utføre natthopp ha gjennomgått teorikurs og avlagt prøve overfor Instruktør 1.</p><p>Godkjenning for gjennomført teorikurs skal være attestert for i hopperens loggbok.</p>',
	),

'103' => array('title' => 'BESTEMMELSER FOR VANNHOPP', 'text' =>
	'For alle fallskjermhopp der det er planlagt landing i vann dypere enn 1 m skal følgende bestemmelser etterfølges:'
	),

'103.3.4' => array('title' => 'KVALIFIKASJONER',
	'text' => '<p>Vannhopp tillates bare utført av hoppere med minst B-sertifikat. Hoppere skal, før de tillates å utføre vannhopp, ha gjennomgått teorikurs og avlagt prøve overfor Instruktør 1.</p><p>Godkjenning for gjennomført teorikurs skal være attestert for i hopperens loggbok.</p>',
	),

'103.4' => array('title' => 'BESTEMMELSER FOR OPPVISNINGSHOPP'),

'103.4.5' => array('title' => 'AVSTAND TIL PUBLIKUM ',
	'text' => 'Avstand mellom landingsområde og publikum skal være større enn 15 meter horisontalt og 25 fot vertikalt. Jfr BSL D 4-3.'),

'103.5' => array('title' => 'BESTEMMELSER FOR TANDEMHOPPING'),

'103.5.4' => array('title' => 'UTSPRANGS- OG TREKKHØYDE',
	'text' => 'For tandemhopping er laveste tillatte utsprangshøyde 7500 fot, og laveste trekkhøyde 5000 fot.'
	),

'200' => array('title' => 'MATERIELLREGLEMENT'),

'201' => array('title' => 'GENERELT'),

'201.4' => array('title' => 'OVERHOLDELSE/DISIPLINÆRTILTAK', 'text' =>
	'Alle som utøver materielltjeneste etter disse bestemmelser plikter å kjenne de deler av bestemmelsene som angår vedkommendes oppgaver, og plikter å overholde dem på alle punkter.'
	),

'201.4.1' => array('title' => 'PÅTALE AV BRUDD',
	'text' => '<p>Et hvert medlem av Fallskjermseksjonen/NLF har rett og plikt til å gripe inn ved brudd på Materiellreglementets bestemmelser.</p><p>Brudd på bestemmelsene i Del 200 er rapporteringspliktige jfr Del 500.</p>',
	),

'202' => array('title' => 'ORGANISASJON'),

'202.4' => array('title' => 'KLUBBSTYRET', 'text' =>
	'Det lokale klubbstyre har uten unntak ansvaret for at klubbenes materielltjeneste gjennomføres i samsvar med gjeldende bestemmelser.'
	),

'202.7' => array('title' => 'EIER AV MATERIELL', 'text' =>
	'Eier av materiell er selv ansvarlig for dette. Eier av fallskjerm betraktes som skjermens materiellforvalter. Hovedkontroll, reservepakking og eventuell annen ønsket kontroll foretas av Materiellkontrollør.'
	),

'206' => array('title' => 'PAKKING', 'text' =>
	'<p>Selvstendig pakking kan kun gjøres av de som har gyldig pakkersertifikat for vedkommende kategori fallskjerm, ref del 300. Pakkekontroll skal utføres etter bestemmelser i Materiellhåndboken.  Pakking skal skje iht.  fallskjermprodusentens pakkemanual. </p>'.
	'<p>Fallskjermens løftestropper skal være festet til seletøy/pakksekk når fallskjermen pakkes.</p>'.
	'<p>Foreligger det mistanke om at væte, urenheter etc kan ha trukket gjennom/inn i pakksekken skal denne åpnes og skjermen kontrolleres. Skjermen tørkes og renses før pakking i de tilfeller hvor dette måtte vise seg nødvendig.</p>',
	'<p>Åpen ild skal ikke forekomme innenfor 50 m avstand fra fallskjermmateriell.</p>'
	),

'206.1' => array('title' => 'OMPAKKINGSSYKLUS', 'text' =>
	'Hovedfallskjermer, reserveskjermer i elevutstyr, reserveskjermer i tandemutstyr og reserveskjermer i andre felleseide fallskjermsett som har vært lagret i pakket stand i mer enn 6 måneder, er ikke luftdyktig.  Reservefallskjermer i privateid sportsutstyr kan lagres i 12 måneder.  For igjen å bli luftdyktig må fallskjermen luftes ved at den åpnes og ristes grundig før den pakkes på ny. '
	),

'206.2' => array('title' => 'PAKKEKONTROLL'),

'206.2.1' => array('title' => 'VINGSKJERMER', 'text' =>
	'For vingskjermer foretas pakkekontrollen slik:'.
	'<ul>'.
	'<li>1. kontroll: Når kalotten er lagt ned på siden med linene og kalotten strukket ut (gjelder ikke ved pro-pakking).</li>'.
	'<li>2. kontroll: Når kalotten er brettet sammen, panelene trukket ut, halen lagt, slideren trukket opp.</li>'.
	'<li>3. kontroll: Når kalotten er brettet sammen og lagt i bagen, linene sløyfet inn og bagen lagt inn i pakksekken.</li>'.
	'<li>4. kontroll: Når sekken er lukket. </li>'.
	'</ul>',
	),

'207' => array('title' => 'KONTROLL AV FALLSKJERMUTSTYR', 'text' =>
	'Kontroll av fallskjermutstyr omfatter <ul><li>Hovedkontroll</li><li>Brukskontroll</li></ul>'
	),

'207.1' => array('title' => 'HOVEDKONTROLL'),

'207.1.2' => array('title' => 'INTERVALLER', 'text' =>
	'Fallskjermsett skal underkastes hovedkontroll:'.
	'<ul>'.
	'<li>Før fallskjermsettet kan tas i bruk (førstegangs kontroll).</li>'.
	'<li>Innen 6 måneder etter siste hovedkontroll for elevfallskjermsett, tandemfallskjermsett og fallskjermsett som er felleseid klubbutstyr. </li>'.
	'<li>Innen 12 måneder etter siste hovedkontroll for annet utstyr.</li>'.
	'<li>Etter utført større reparasjon.</li>'.
	'<li>Etter feilfunksjonert hovedskjerm.</li>'.
	'<li>Etter aktivert reserveskjerm (kontrolleres sammen med seletøy/pakksekk.)</li>'.
	'<li>Når det er ønskelig av ansvarlig pakker, materiellforvalter eller eier.</li>'.
	'</ul>'.
	'<p>'.
	'Elevfallskjermsett, tandemfallskjermsett og fallskjermsett som er felleseid klubbutstyr er luftdyktig i 3 måneder etter gjennomført hovedkontroll. Deretter må brukskontroll iht 207.2 gjennomføres for at utstyret skal være luftdyktig fram til neste forfall for hovedkontroll.'.
	'</p>'
	),

'208' => array('title' => 'BESTEMMELSER SOM SKAL FLYTTES TIL MATERIELLHÅNDBOKEN'),

'208.2' => array('title' => 'KASSASJON AV FALLSKJERMER'),

'208.2.3' => array('title' => 'KASSASJONSGRUNNLAG'),

'208.2.3.2' => array('title' => 'FALLSKJERMER SOM HAR VÆRT I KONTAKT MED SJØVANN (SALTVANN)', 'text' =>
	'Komponenter som har vært i kontakt med sjøvann (saltvann), i mer enn 48 timer, eller som ikke har blitt skyllet tilfredsstillende innen 24 timer etter berging, skal kasseres.'
	),

'208.2.4' => array('title' => 'ANVENDELSE AV KASSERT MATERIELL', 'text' =>
	'<p>Kryss skal males diagonalt over pakksekken, og over skjermens serienummer. Bruk kontrastfarget maling.</p>'.
	'<p>Kasserte fallskjermer (øvingsmateriell) skal lagres separat fra luftdyktig utstyr.</p>'
	),

'208.5' => array('title' => 'RESERVESKJERMEN', 'text' => 
	'<p>Reserveskjermen er en rund kalott eller vingfallskjerm. Kun skjermer som er produsert for anvendelse som nødskjerm (low speed type parachute) kan godkjennes. Runde kalotters synkehastighet skal ikke overskride 7,5 m/sek ved en belastning på 75 kg vekt. </p>'.
	'<p>Kategoriinndeling er som beskrevet i Del 300.</p>'.
	'<p>Reserveskjermen består av pilotskjerm med bånd, innerbag/diaper, kalott med bæreliner og links (sjakler), utløserhåndtak med kabel og pinne(r), samt pakkerlogg.</p>'.
	'<p>Pilotskjermen skal være godkjent av seletøyprodusenten, og festes til runde skjermers toppliner ved hjelp av pilotbåndet. Ved vingreserver skal riggprodusentens fribagsystem nyttes. Type bånd, lengde og festemetode er beskrevet i Materiellhåndboken. </p>'.
	'<p>Seletøy skal være utstyrt med tilkoplet LOR/RSL line slik det er beskrevet av produsenten eller av annen godkjent type for automatisk aktivering av reserveskjerm ved cut away fra utløst, ikke flyvende hovedskjerm.</p>'.
	'<p>Dersom typegodkjent nødåpner vedlikeholdt iht. produsentens krav er montert og i bruk på reserveskjermen kan LOR/RSL-line demonteres. </p>'
	),

'209' => array('title' => 'ANNET FALLSKJERMMATERIELL - FLYTTES TIL MATERIELLHÅNDBOKEN'),

'209.1' => array('title' => 'UTLØSERLINE', 'text' =>
	'Linen skal ha en bruddstyrke på minst 1000 kg, og være 3,90 m lang om ikke annet er bestemt av produsenten eller blir fastsatt for bruk i forbindelse med bestemt flytype. Utløserlinen skal ha godkjent type ankerkrok med sikringspinne. Utløserlinen skal ha krum pinne, eller annen godkjent anordning for låsing av pakksekk. Ved utsprang fra stag skal utløserlinen være innsydd til halv bredde for å redusere vindfang. '
	),

'213' => array('title' => 'GODKJENNING / VEDLIKEHOLD AV ANNET MATERIELL - FLYTTES TIL MATERIELLHÅNDBOKEN'),

'213.2' => array('title' => 'FLYTEVEST', 'text' =>
	'<p>Flytevester skal være godkjent av Materiellkontrollør. Godkjenning skal skje iht følgende:</p>'.
	'<ul>'.
	'<li>Vesten kan være oppblåsbar eller av fast materiale.</li>'.
	'<li>Vesten skal under ingen omstendighet kunne være til fare for hopperen eller til hinder for utløsning av fallskjermen eller frigjøring fra seletøyet.</li>'.
	'<li>Vesten skal ha en oppdrift på minimum 50 Newton.</li>'.
	'<li>Oppblåsbare vester uten trykkpatron kan godkjennes hvis ventil for innblåsing av luft vurderes å være lett tilgjengelig under bruk.</li>'.
	'<li>Oppblåsbare vester med trykkpatron kan godkjennes hvis aktiveringsmekanisme vurderes å være lett tilgjengelig under bruk.</li>'.
	'<li>Flytevester som er spesielt ømfintlige for slitasje kan anvendes når de er utstyrt med overtrekk.</li>'.
	'</ul>'.
	'<p>Felleseide flytevester skal underkastes kontroll av Materiellkontrollør når det anses nødvendig, dog minst en gang årlig. </p>'
	),

'300' => array('title' => 'FALLSKJERMSERTIFIKATER'),

'300.1' => array('title' => 'FRITT FALL', 'text' =>
	'Med fritt fall menes her et hopp der hopperen selv utløser sin skjerm etter at han har forlatt luftfartøyet.'
	),
	
'301' => array('title' => 'GENERELT'),

'301.1' => array('title' => 'UTSTEDELSE', 'text' =>
	'<p>F/NLFs fallskjermsertifikater er kompetansebevis som utstedes av Sikkerhets- og utdanningskomiteen F/NLF til medlemmer av F/NLF og lokalklubbene etter søknad påført attestasjon om at kravene er oppfylt. Etter utstedelse av sertifikat plikter søkeren i gyldighetsperioden til enhver tid å underkaste seg doping- eller rusmiddelkontroll, herunder utvidet blodprøve. </p>'.
	'<p>Søknader om utstedelse av elevbevis, fallskjermsertifikat eller lisens sendes NLFs sekretariat, påført attestasjon av Instruktør 1 eller høyere. Instruktøren er ansvarlig for at søknaden samsvarer med gjeldene vedlikeholdskrav.  Søknader om fornyelse sendes NLFs sekretariat, påført attestasjon av Instruktør 2 eller høyere.</p>'.
	'<p>Fallskjermpakkersertifikat utstedes etter anbefaling fra Instruktør 1 eller Materiellkontrollør, med bekreftelse om tilfredsstillende gjennomført eksamensprøve.</p>'.
	'<p>Alle sertifikater er nummerert, og NLF fører register over alle utstedelser og fornyelser.</p>'
	),

'301.2' => array('title' => 'GYLDIGHET', 'text' =>
	'<p>Sertifikatene utstedes for livstid, men er kun gyldig for deltakelse i praktisk hopping i den perioden de er utstedt eller fornyet for, og ved samtidig gyldig og betalt medlemskap i F/NLF.  Kriterier for medlemskap, straffeforføyninger og gjenopptak er gitt i NIFs og NLFs lover. </p>'.
	'<p>Sertifikat anses gyldig ved postlegging av skjema for utstedelse eller fornyelse, dersom dette er bevitnet av instruktør 2 eller høyere.</p>'.
	'<p>Fallskjermpakkersertifikat er automatisk gyldig dersom innehaveren også har gyldig fallskjermsertifikat av klasse A eller høyere. Ellers gjelder vedlikeholdsbestemmelsene i pkt 304.8.</p>',
	),

'301.5' => array('title' => 'FORNYELSE', 'text' =>
	'Fornyelse av sertifikater utføres av NLFs sekretariat etter søknad med attestasjon av Instruktør 2 eller høyere for at fornyelseskravene er oppfylt. Fornyelse skjer for 365 dager.  Instruktør kan ikke fornye egne sertifikater.'
	),

'308' => array('title' => 'A-SERTIFIKAT'),
'308.1' => array('title' => 'KVALIFIKASJONS- OG FERDIGHETSKRAV'),
'308.1.1' => array('title' => 'MED BAKGRUNN I GRUNNKURS, LINE', 'text' =>
	'Fullført progresjonsplan iht Del 600. Minst 20 godkjente fritt fall hopp. Minst 10 hopp med landing innenfor 50 meter. Utsjekk firkantreserve, utsjekk høyverdig firkantskjerm.  Deltatt på A-sertfikatkurs under ledelse av instruktør 2. Inneha pakkesertifikat for hovedskjerm kategori 1.  Avlagt teoriprøve med tilfredsstillende resultat overfor Instruktør 1, i Håndbokas Del 100, Del 200 og Del 500. '
	),

'308.2' => array('title' => 'RETTIGHETER OG BEGRENSNINGER', 'text' =>
	'<p>A-sertifikat gir innehaveren rett til å gjennomføre solo fritt fall under ledelse og kontroll av Hoppleder, og til å hoppmestre seg selv på solo fritt fall hopp.  A-sertifikat gir hopperen rett til å hoppe med typegodkjent privateid fallskjermsett (fallskjermsett som ikke er elevskjermsett).</p>'.
	'<p>Innehaver av A-sertifikat kan kun delta i formasjonshopping (FS, VFS) under praktisk opplæring av instruktør.  Formasjonene må ikke bestå av mer enn totalt tre hoppere. </p>'.
	'<p>Innehaveren betegnes "Solo frittfall hopper / parachutist".</p>'
	),

'308.3' => array('title' => 'FORNYELSESKRAV'),

'308.3.1' => array('title' => 'GENERELT KRAV', 'text' =>
	'<p>Gjennomført minimum 20 fallskjermhopp siste 365 dager.</p>',
	'<p>HI kan unntaksvis og etter individuell vurdering fornye A-sertifikat med færre enn 20 hopp de siste 365 dager.  Dersom dette gjøres skal HI vedlegge en særskilt vurdering når fornyelsespapirene sendes til F/NLF.</p>',
	),

'309' => array('title' => 'B-SERTIFIKAT', 'text' => ''),

'309.1' => array('title' => 'KVALIFIKASJONS- OG FERDIGHETSKRAV', 'text' =>
	'<p>Inneha A-sertifikat.  Minst 50 godkjente fritt fall hopp.  Mer enn 30 minutter akkumulert frittfall tid.</p>'.
	'<p>Fullført progresjonshopp for A-sertifikat omfattende skjermkontroll og sikkerhet under formasjonshopping (FS og VFS).</p>',
	'<p>Avlagt skriftlig prøve overfor Instruktør 1 omfattende Håndbokens del 100, 200, 300, 400 og 500.</p>'
	),

'309.2' => array('title' => 'RETTIGHETER OG BEGRENSNINGER', 'text' =>
	'<p>B-sertifikatet gir innehaveren rett til å delta i praktisk hopping under ledelse og kontroll av Hoppleder, og til å hoppmestre seg selv.  Det gir rett til å delta i konkurranser, nasjonalt og internasjonalt.  B-sertifikatet gir også rett til å attestere i hopplogg for andre hoppere med A-sertifikat eller hoyere, dog ikke for de spesielle progresjonshopp som skal kontrolleres av Instruktor.</p>'.
	'<p>B-sertifikatet gir hopperen rett til å fungere som Hoppfeltleder, med plikter og rettigheter iht Del 500.</p>'.
	'<p>Innehaveren betegnes "Hoppfeltleder / <i>freefall parachutist</i>".</p>'
	),
	
'309.3' => array('title' => 'FORNYELSESKRAV', 'text' => 'Som for A-sertifikat.'),

'310' => array('title' => 'C-SERTIFIKAT'),

'310.1' => array('title' => 'KVALIFIKASJONS- OG FERDIGHETSKRAV', 'text' =>
	'<p>Inneha B-sertifikat.  Fylt 18 år. 200 godkjente fritt fall hopp.  Minst 50 formasjonshopp (FS og/eller VFS) hvor minst 10 må ha bestått av hhv fire (FS) eller tre (VFS) eller flere deltagere.</p><p>Mer enn 1 time akkumulert fritt fall tid.</p>'.
	'<p>Ha deltatt i minst 1 års praktisk (aktiv) hoppvirksomhet som selvstendig hopper. Ha gjort tjeneste som HFL minst 5 ganger. Bestått skriftlig prøve overfor Instruktør 1 omfattende Håndbokens del 100, 200, 300, 400 og 500. Av lokal Hovedinstruktør være funnet skikket til å fungere som Hoppleder ved treningshopping for hoppere med minimum A-sertifikat, jfr. Pkt.  310.2.</p>'
	),

'310.2' => array('title' => 'RETTIGHETER OG BEGRENSNINGER', 'text' =>
	'<p>C-sertifikat gir samme rettigheter som B-sertifikat, samt rett til å attestere alle hopplogger, dog ikke for progresjonshopp som krever attestasjon av Instruktør.</p>'.
	'<p>C-sertifikat gir rett til å fungere som Hoppleder ved treningshopping for hoppere med minimum A-sertifikat. </p>'.
	'<p>Innehaveren betegnes "Hoppmester / experienced parachutist".</p>'
	),

'310.3' => array('title' => 'FORNYELSESKRAV', 'text' =>
	'<p>Fornyelse av C-sertifikat krever minimum 40 hopp siste 365 dager.  Om hopperen har gjennomført mer enn 20 fallskjermhopp siste 365 dager kan B-sertifikat fornyes.  C-sertifikat kan fornyes når hopperen har oppnådd 40 hopp siste 365 dager. </p>'.
	'<p>Om hopperen har gjennomført mindre enn 20 hopp siste 365 dager vil fornyelseskrav som for A og B sertifikat gjelde, ref pkt 308.3.</p>',
	),

'311' => array('title' => 'D-SERTIFIKAT'),

'311.1' => array('title' => 'KVALIFIKASJONS- OG FERDIGHETSKRAV', 'text' =>
	'<p>Inneha C-sertifikat og Instruktør 3-lisens.</p>'.
	'<p>Minst 500 godkjente fritt fall hopp.  Mer enn 3 timer akkumulert fritt fall tid.</p>'.
	'<p>Avlagt skriftlig prøve overfor Instruktør 1 omfattende Håndbokens del 100, 200, 300, 400, 500 og 600, med vedlegg for alle kapitler. Ha teoretisk kjennskap til CF-hopping. Prøvene skal være avlagt overfor Instruktør 1. </p>'
	),

'311.2' => array('title' => 'RETTIGHETER OG BEGRENSNINGER', 'text' =>
	'<p>D-sertifikat gir samme rettigheter som C-sertifikatet, samt rett til å godkjenne hoppfelt for egen hopping.</p>'.
	'<p>D-sertifikat gir hopperen rett til å fungere som Hoppleder ved alminnelig hopping.</p>'.
	'<p>Innehaveren betegnes "Hoppleder / senior parachutist".</p>'
	),

'312' => array('title' => 'GJENUTSTEDELSE AV A-, B-, C- ELLER D- SERTIFIKAT NÅR DET ER HOPPET DE SISTE 10 ÅR'),

'313' => array('title' => 'VIDEOSERTIFIKAT'),

'313.1' => array('title' => 'VIDEOSERTIFIKAT TANDEM'),

'313.1.2' => array('title' => 'KVALIFIKASJONSKRAV', 'text' =>
	'<p>C-sertifikat.  Utført minimum 50 hopp som fotograf for FS.  Før kursstart skal resultatet av 20 av hoppene/ferdighetene presenteres på DVD.</p>'.
	'<p style="color:red;">Ved kursstart skal nye kandidater presentere resultatet av 20 av disse fotohoppene for vurdering av ferdighetene til å filme på magen i minimum 1:1 i front slik tandem vil være filmet.  For bedre å få inn dette med kompetanse, ferdigheter og kunnskap skal kurset holdes av en erfaren VT hopper sammen med en I-1 som har eller har vært instruktør tandem.</p>'
	),

'314' => array('title' => 'DEMOSERTIFIKATER', 'text' => 'Demosertifikater utstedes i tre klasser.'),

'314.1' => array('title' => 'DEMOSERTIFIKAT II'),
'314.1.1' => array('title' => 'KVALIFIKASJONSKRAV', 'text' =>
	'C-sertifikat og særlig vurderte personlige egenskaper og dyktighet. Minimum 300 hopp.  Vurdering foretas av lokal Hovedinstruktør. Særlig utsjekk på hopp med utstyr etter Del 100. '
	),

'314.2' => array('title' => 'DEMOSERTIFIKAT I'),
'314.2.1' => array('title' => 'KVALIFIKASJONSKRAV', 'text' =>
	'D-sertifikat samt å ha deltatt i minst 10 oppvisninger. Inneha demosertifikat II. Særlig vurderte personlige egenskaper og dyktighet. Vurdering foretas av lokal Hovedinstruktør.'
	),

'314.2.2' => array('title' => 'RETTIGHETER', 'text' => 'Lede gjennomføringen av en fallskjermoppvisning.'),
'314.2.4' => array('title' => 'VEDLIKEHOLDSKRAV', 'text' => 'Inneha gyldig D-sertfikat.'),

'314.3' => array('title' => 'DEMOSERTIFIKAT TANDEM'),
'314.3.1' => array('title' => 'KVALIFIKASJONSKRAV', 'text' =>
	'Godkjent Tandeminstruktør. Inneha Demosertifikat I. Ha vært godkjent Tandeminstruktør i minst 1 år. Ha gjennomført minst 200 Tandemhopp. Inneha særlige personlige egenskaper og dyktighet. Anbefaling gis av lokal Hovedinstruktør. Sikkerhets- og utdanningskommiteen vurderer og godkjenner. Avslag kan gis uten begrunnelse. '
	),

'314.3.2' => array('title' => 'RETTIGHETER', 'text' => 'Skal lede oppvisninger der Tandemhopp inngår som en del av oppvisningen.'),
'314.3.4' => array('title' => 'VEDLIKEHOLDSKRAV', 'text' =>
	'Gyldig D-sertifikat.  Gyldig demosertifikat klasse I. Ha gjennomført minst 50 tandemhopp siste 365 dager'
	),

'400' => array('title' => 'INSTRUKTØRLISENSER'),

'401' => array('title' => 'GENERELT'),

'401.2' => array('title' => 'KANDIDATER', 'text' =>
	'Sikkerhets- og Utdanningskomiteen F/NLF vurderer og tar ut instruktørkandidater til klasse 2 eller høyere, etter søknad og anbefaling fra lokalklubbens Hovedinstruktør. Personer som ønsker å bli antatt som kandidater leverer søknad gjennom lokalklubbs Hovedinstruktør innen de tidsfrister som SU fastsetter. Instruktørkandidater av klasse 3 vurderes og uttas av lokalklubbens Hovedinstruktør.'
	),

'401.5' => array('title' => 'FORNYELSE'),

'401.5.2' => array('title' => 'INSTRUKTØR / EKSAMINATOR', 'text' =>
	'For Instruktør/Eksaminator gjelder ingen fornyelseskrav. SU vil benytte vedkommendes kompetanse ved behov. '),

'402' => array('title' => 'KLASSER OG BETEGNELSER'),

'402.1' => array('title' => 'INSTRUKTØR 3 (I-3)', 'text' =>
	'Instruktør 3 er en fallskjermhopper som innehar Instruktørlisens 3, og som er godkjent av Hovedinstruktør til å hoppmestre elever på line, fritt fall og AFF-elever fra nivå 8, samt til å attestere alle hopplogger, dog ikke for spesielle progresjonshopp som krever godkjenning av instruktør av høyere klasse. '
	),

'402.1.1' => array('title' => 'ERFARINGSKRAV', 'text' =>
	'Inneha norsk C-sertifikat. Ha gjort tjeneste som HFL ved hopping med elever minst 5 ganger. Ha vært hjelpeinstruktør på minst 3 grunnkurs del 1 og minst 2 grunnkurs del 2.'
	),

'402.2' => array('title' => 'INSTRUKTØR 2 (I-2)'),

'402.2.1' => array('title' => 'ERFARINGSKRAV', 'text' =>
	'Ha deltatt i til sammen 2 års praktisk hoppvirksomhet, herav minst 12 mndr som Instruktør 3, og derigjennom tilegnet seg allsidig erfaring i hoppmestring av elever, samt erfaring med hoppfeltorganisasjon (HFL- tjeneste). Ha vært hjelpeinstruktør på minst 2 komplette grunnkurs (både del 1 og del 2), etter utstedt Instruktørlisens 3. '
	),

'402.3' => array('title' => 'INSTRUKTØR 1 (I-1)'),

'402.3.1' => array('title' => 'ERFARINGSKRAV', 'text' =>
	'Ha deltatt i minst 3 års praktisk hoppvirksomhet, herav minst 12 mndr som I-2 og derigjennom ha tilegnet seg allsidig erfaring i grunnleggende og videregående utdannelse og kontroll av elever på alle trinn, samt erfaring som Hoppleder. Som Instruktør 2 ha vært instruktør ved minst 3 komplette grunnkurs frem til elevenes FF-status, dvs både grunnkursets del 1 og del 2. Inneha norsk D-sertifikat og Demosertifikat.'
	),


'402.3.3' => array('title' => 'RETTIGHETER OG OPPGAVER', 'text' =>
	'Etter fastlagte programmer iht Del 600 og retningslinjer gitt av lokal Hovedinstruktør, selvstendig legge opp, lede og gjennomføre fallskjermkurs og videregående utdannelse av hoppere frem til og med D-sertifikat. Instruere i vannhopp, natthopp og oksygenhopp, samt all annen alminnelig hopping. Instruere i CF dersom dette er gjennomført selv. Undervise, kontrollere og autorisere fallskjermpakkere på alle typer sportsskjermer som han selv er sertifisert for. Være eksaminator og sensor ved prøver av alle typer og grader, unntatt F/NLFs B-kurs, C-kurs, MK-kurs, AFF-kurs og Tandemkurs. Være HL for alle typer hopping.  Rekognosere og anbefale godkjenning av hoppfelt for alle typer hopping. Som HL godkjenne hoppfelt etter gjeldende bestemmelser.'
	),

'402.4' => array('title' => 'INSTRUKTØR TANDEM'),

'402.4.1' => array('title' => 'ERFARINGSKRAV', 'text' =>
	'<p>Inneha instruktørlisens 2 eller høyere. Ha vært aktiv hoppmester under hopping med elever.</p>'.
	'<p>Minimum 100 hopp siste år før utsjekk.</p>'.
	'<p>Under kontroll og oppfølging fra egen Hovedinstruktør ha gjennomført 20 utviklingshopp i løpet av 12 måneder, derav minst 4 hopp i løpet av de første 3 månedene etter gjennomført Tandemhoppmesterkurs.</p>'
	),

'500' => array('title' => 'OPERATIVE BESTEMMELSER'),

'501' => array('title' => 'GENERELT', 'text' =>
	'<p>Del 500 av F/NLF Bestemmelser fastlegger forutsetningene for utøvelse av utdanning og praktisk hoppvirksomhet, og foreskriver hvordan organisering og drift skal skje. </p>'.
	'<p>Praktisk hoppvirksomhet er å forstå som all virksomhet som inkluderer utsprang fra luftfartøy i luften - elevhopping, treningshopping, konkurransehopping og alle spesielle hopptyper som oppvisnings-, natt-, vann- og oksygenhopping, etc.</p>'.
	'<p>All utdanning og praktisk hoppvirksomhet skal skje etter denne F/NLF håndbok som er godkjent som sikkerhetssystem etter Luftfartstilsynets BSL- D 4.2 som regulerer all sivil fallskjermhopping i Norge.</p>'.
	'<p>Del 500 beskriver dessuten det ansvar og den myndighet de enkelte ledd i den operative organisasjonen har, og gir instrukser for det operative personellet (se vedlegg 1-4).</p>'.
	'<p>Ethvert medlem som befinner seg på hoppfeltet, plikter å rette seg etter de anvisninger som gis av ansvarlig personell. Ethvert klubbmedlem har rett og plikt til å gripe inn ved brudd på bestemmelsene, ref Del 100 og del 200. </p>'
	),

'502' => array('title' => 'ORGANISASJON'),

'502.1' => array('title' => 'SIKKERHETS- OG UTDANNINGSKOMITEEN (SU)'),

'502.2' => array('title' => 'MATERIELLSJEFEN (MSJ)', 'text' =>
	'<p>MSJ oppnevnes av Styret F/NLF og utøver i samarbeid med SU overordnet kontroll med all materielltjeneste innen F/NLF. MSJ utarbeider årlig målsettinger og en arbeidsplan som godkjennes av Styret F/NLF. </p>'.
	'<p style="color:red;">Med hjemmel i Bestemmelser for Sivil Luftfart (BSL) D 4-2 § 4 (2) b, utsteder, iverksetter og håndhever SU gjennom MSJ og F/NLFs Håndbok Del 200 og Materiellhåndboka, bestemmelser for F/NLFs materielltjeneste.</p>'.
	'<p>MSJ typegodkjenner alt fallskjermmateriell.</p>'
	),

'502.3' => array('title' => 'HOVEDINSTRUKTØR (HI)', 'text' =>
	'<p>Hovedinstruktør er den Instruktør som er oppnevnt av styret i klubb eller annen organisasjonsenhet til å lede all utdanning og praktisk hoppvirksomhet.  Oppnevnelsen skal godkjennes av SU. Klubbens operasjonstillatelse er avhengig av Hovedinstruktørens lisensklasse. Dersom spesielle grunner tilsier det, kan SU dispensere fra kravet om HI av klasse I-1 for klubber/enheter som ønsker OT-1.</p>'.
	'<p>HI har ansvaret for at all utdannelse og praktisk hopping skjer etter en plan godkjent av klubbstyret, og i henhold til de til enhver tid gjeldende bestemmelser.  Planen skal inneholde delmål og resultatmål, samt planer for hvordan klubbens operative drift skal koordineres, gjennomføres og kontrolleres. Planen skal som et minimum inneholde målsettinger for hovedområdene hoppfeltdrift, utdanning, materielltjeneste, kommunikasjon og kvalitetskontroll.</p>'.
	'<p>HI organiserer og har ansvar for all praktisk hopping og utdanning som klubben har driftstillatelse for gjennom fordeling av oppgaver til sin operative stab av instruktører, hoppledere, hoppmestere og hoppfeltledere, med tilfredsstillende kvalifikasjoner.</p>'.
	'<p style="color:red;">HI godkjenner hoppfelt for alminnelig hopping i henhold til klubbens operasjonstillatelse og innhenter grunneiers tillatelse, varsler politi og lufttrafikktjeneste og ivaretar koordinering mot relevant myndighet.  Godkjenning av hoppfelt for alminnelig hopping til varig bruk varsles SU som gjennom kontakt med Luftfartstilsynet påser at hoppfeltet opptas i AIP Norge, ENR 5.5-4 Fallskjermhopping faste steder. </p>'.
	'<p>Hovedinstruktøren er faglig ansvarlig for sikkerhet og materiell. Han er faglig bindeledd mellom lokalklubb og SU. </p>'.
	'<p>Hovedinstruktøren har ansvaret for klubbens operative arkiv, manifester, kursprotokoller, NOTAM, korrespondanse med SU og F/NLF. Han er ansvarlig for at rutinemessige rapporter som vedrører den operative drift blir utarbeidet og innsendt i rett tid, samt at hendelsesrapporter blir utarbeidet, kommentert og rapportert til SU v/ F/NLFs sekretariat uten unødig opphold.</p>'
	),

'502.4' => array('title' => 'INSTRUKTØr (I)', 'text' =>
	'Instruktør ivaretar og utforer de oppgaver og funksjoner som etter disse bestemmelser krever instruktørkvalifikasjoner, med de rettigheter og begrensninger som gjelder for den enkelte lisensklasse.'
	),
	
'502.5' => array('title' => 'HOPPLEDER (HL)', 'text' =>
	'HL er den person som er overlatt ansvaret for gjennomforing av praktisk hoppvirksomhet på ETT STED i et BESTEMT TIDSROM.  HL skal være utpekt før hopping startes av lokal HI.  HL skal folge Hopplederinstruksen, se vedlegg 1 til del 500.'
	),
	
'502.6' => array('title' => 'HOPPFELTLEDER (HFL)', 'text' =>
	'HFL utpekes av HL, er underlagt denne, og har til oppgave å administrere, organisere og overvåke hoppfeltet, og påse at hoppingen gjennomføres etter gjeldende bestemmelser. HFL skal følge Hoppfeltlederinstruks, se vedlegg 2 til del 500.'
	),

'502.10' => array('title' => 'KOMBINASJON AV FUNKSJONER ', 'text' =>
	'<p>HFL tillates ikke å hoppe, og skal under hopping til enhver tid være tilstede på hoppfeltet. Funksjonen som HL kan kombineres med funksjon som HFL, HM eller Flyger.</p>'.
	'<p>HL kan dersom aktiviteten krever det utpeke flere HFL-er, som skal virke sammen. HL har ansvar for oppgavefordelingen mellom disse.</p>'
	),

'503' => array('title' => 'OPERASJONSTILLATELSER'),

'503.2' => array('title' => 'OPERASJONSTILLATELSE KLASSE 2 (OT 2)', 'text' =>
	'<p>Organisasjon som innehar OT 2 kan organisere og gjennomføre fallskjermhopping for hoppere med A-sertifikat og høyere, innenfor et bestemt geografisk område, men ikke teoretisk eller praktisk utdanning i fallskjermhopping som definert i Del 600, eller spesielle hopptyper som definert i Del 100.</p>'.
	'<p>OT 2 utstedes til klubber eller organisasjoner som kan dokumentere at de tilfredsstiller følgende krav: </p>'.
	'<ul>'.
	'<li>For klubber: Er opptatt både i NLF/NAK.</li>'.
	'<li>For driftsorganisasjoner: Er godkjent av F/NLF.</li>'.
	'<li>Har Hovedinstruktør klasse I/2 godkjent av SU.</li>'.
	'<li>Disponerer hoppfelt iht Del 100.</li>'.
	'</ul>'
	),

'504' => array('title' => 'KLARERING AV AKTIVITET - NOTAM'),

'504.2' => array('title' => 'NOTAM', 'text' =>
	'<p>For fallskjermhopping utenfor områder med obligatorisk radiosamband med flygekontrolltjenestens enheter, som enten kan sies å være;</p>'.
	'<ol>'.
	'<li>av større omfang og vil foregå utenfor kunngjorte, faste hoppfelt, eller</li>'.
	'<li>vil foregå om natten,</li>'.
	'</ol>'.
	'<p>skal det sendes inn NOTAM i h.t. pkt 504.2.1.</p>'.
	'<p>Som hopping av større omfang menes mer enn 20 hopp per dropp, mer enn 100 hopp totalt og/eller aktivitet utover 24 timer.</p>'
	),

'505' => array('title' => 'HANDLINGSINSTRUKS VED ULYKKE'),

'505.1' => array('title' => 'PRIORITERTE TILTAK', 'text' =>
	'<table>'.
	'<tr><td>0.</td><td>Start førstehjelp.</td><td>&nbsp;</td></tr>'.
	'<tr><td>1.</td><td>Tilkall lege:</td><td>tlfnr:....................</td></tr>'.
	'<tr><td>2.</td><td>Tilkall politi:</td><td>tlfnr:....................</td></tr>'.
	'<tr><td>3.</td><td>Varsle Flygekontrollenhet:</td><td>tlfnr:....................</td></tr>'.
	'<tr><td>4.</td><td>Varsle F/NLFs sentrale organisasjon v/Fagsjef:</td><td>tlfnr:....................</td></tr>'.
	'<tr><td>&nbsp;</td><td>evt. Leder F/NLF:</td><td>tlfnr:....................</td></tr>'.
	'<tr><td>&nbsp;</td><td>evt. Leder SU:</td><td>tlfnr:...................</td></tr>'.
	'<tr><td>&nbsp;</td><td>som vil sørge for at sentrale tiltak iverksettes.</td></td>&nbsp;<td></tr>'.
	'<tr><td>5.</td><td>Varsle egen Hovedinstruktør:</td><td>tlfnr:....................</td></tr>'.
	'</table>'.
	'<p>NB! Det er politiets ansvar å underrette forulykkedes pårørende.</p>'
	),

'505.2' => array('title' => 'FORHOLD TIL PRESSE/MEDIA', 'text' =>
	'<p>Avstå fra intervju på ulykkesdagen. Bruk om nødvendig denne standarduttalelse:</p>'.
	'<ul><li>"Ulykken vil bli etterforsket av politiet og av kommisjon fra Norges Luftsportsforbund. Vi avstår fra å kommentere årsaksforholdet inntil kommisjonens rapport foreligger."</li></ul>'.
	'<p>NB! Oppgi IKKE forulykkedes navn eller andre personalia. Det er politiets ansvar. </p>'.
	'<p>Pass på at andre på feltet ikke kommer med egne uttalelser som er i strid med de ovenstående retningslinjer.</p>'.
	'<p>Ved særlig pågående pressefolk:</p>'.
	'<p>Gjør disse oppmerksom på "vær varsom"-paragrafen som er trykket på deres eget pressekort. Der står det blant annet:</p>'.
	'<ul><li>"Vis særlig hensyn overfor personer som ikke kan ventes å være klar over bivirkningen av sine uttalelser. Misbruk ikke andres følelser, uvitenhet eller sviktende dømmekraft." </li></ul>'
	),

'506' => array('title' => 'HOPPLEDERINSTRUKS'),

'506.2' => array('title' => 'KVALIFIKASJONER', 'text' =>
	'<p>Som tilstrekkelig kvalifikasjon for HL regnes:</p>'.
	'<ul>'.
	'<li>For hopping med hoppere med A-sertifikat og høyere: C-sertifikat </li>'.
	'<li>Ved hopping med elever: D-sertifikat eller Instruktør 2. <p style="color:red;">HI kan unntaksvis og etter særskilt vurdering for elevhopping godkjenne HL med C sert og I-3 når vedkommende minimum har 2 års erfaring som I-3, har deltatt jevnlig i klubbens utdanningsvirksomhet og operative drift i tillegg til å ha inngående lokalkunnskaper. Dette gjelder kun ved hopping fra fly med maks 6 hoppere og kun ved dropp fra et fly. Godkjente HL-er ihht dette, skal framkomme i HIs plan</p></li>'.
	'<li>Ved natt-, vann- og oksygenhopping med personell som ikke har utført slike hopp tidligere:  Instruktør 1</li>'.
	'<li>Ved natt-, vann- og oksygenhopping med personell som har utført slike hopp tidligere:  D-sertifikat eller Instruktør 2 </li>'.
	'</ul>'
	),

'506.6' => array('title' => 'ANDRE FORHOLD'),

'506.6.2' => array('title' => 'DELTAGELSE I HOPPING', 'text' =>
	'HL kan under sin tjeneste være på bakken eller i luften og delta aktivt i hoppingen, dog på en slik måte at han hele tiden har kontroll med virksomheten.'
	),

'507' => array('title' => 'HOPPFELTLEDERINSTRUKS'),

'507.2' => array('title' => 'KVALIFIKASJONER', 'text' =>
	'<p>Som tilstrekkelig kvalifikasjon for HFL regnes: </p>'.
	'<p>Ved hopping med elever:</p>'.
	'<ul><li>Fallskjermhoppere med B-sertifikat eller høyere, eller personell som etter HLs vurdering antas å være spesielt kvalifisert gjennom inngående praktisk erfaring under elevhopping. </li></ul>'.
	'<p>Ved hopping med hoppere med A-sertifikat eller høyere:</p>'.
	'<ul><li>Personell som på forhånd er instruert av HL om de oppgaver som påligger HFL.</li></ul>'.
	'<p>Ved natthopp, vannhopp og oksygenhopp: </p>'.
	'<ul><li>Hopper med Instruktørlisens 3 eller høyere</li></ul>'
	),

'508' => array('title' => 'HOPPMESTERINSTRUKS'),

'508.2' => array('title' => 'KVALIFIKASJONER', 'text' =>
	'<p>Som tilstrekkelig kvalifikasjon for HM regnes:</p>'.
	'<ul>'.
	'<li>Når alle ombordværende hoppere har A-sertifikat eller høyere: Hoppere med B-sertifikat </li>'.
	'<li>Når det blant de ombordværende hoppere er line, eller FF elever: Instruktør 3</li>'.
	'<li>For AFF-elever på nivå 1-7:  Instruktør 3-AFF</li>'.
	'<li>Dersom utsprang innebærer natthopp: Hopper med D-sertifikat, som har gjennomført natthopp tidligere, eller hopper med instruktørlisens klasse 2 eller høyere</li>'.
	'<li>Dersom utsprang innebærer oppvisningshopp: Tilfredsstille krav som angitt under F/NLFs bestemmelser Del 300</li>'.
	'</ul>'
	),

'508.3' => array('title' => 'ANSVAR', 'text' =>
	'<p>HM er ansvarlig for hopperne i sitt løft, og har ansvaret overfor Flyger for at dennes anvisninger blir fulgt.</p>'.
	'<p>HM har ansvaret for at de utsprang som utføres under hans kommando skjer i overensstemmelse med F/NLFs bestemmelser Del 100.</p>'.
	'<p>Der jordtegn anvendes som signalsystem er HM ansvarlig for at utsprang ikke foretas dersom jordtegn er fjernet eller slik plassert at det må forstås at utsprang ikke skal finne sted.</p>'.
	'<p>Er jordtegn eneste kommunikasjon bakke/fly skal HM påse at utsprang ikke gjennomføres uten at jordtegn er synlige fra flyet.</p>'.
	'<p>Anvendes annet kommunikasjonssystem enn jordtegn, har HM ansvaret for at ordrer fra bakken følges.</p>'
	),

'600' => array('title' => 'PROGRAM FOR STANDARDISERT GRUNNOPPLÆRING I FALLSKJERMHOPPING'),

'603' => array('title' => 'PROGRESJONSPLAN'),

'603.3' => array('title' => 'HOPP MED FRITT FALL OG MANUELT UTLØST FALLSKJERM'),

'603.3.1' => array('title' => 'FARLIG/UKONTROLLERT HOPPING OG VURDERING AV PERMANENT HOPPFORBUD', 'text' =>
	'<p>Følgende kriterer er faste normer for kjennelse "Farlig/Ukontrollert" hopping:</p>'.
	'<ul>'.
	'<li>Fosterstilling gjennom hele fallet.</li>'.
	'<li>Tumbling (kraftig rotasjon i flere plan) i fallet og i trekket.</li>'.
	'<li>Feil eller manglende nødprosedyre.</li>'.
	'<li>Direkte trekk uten cutaway ved feilfunksjon av hovedskjerm.</li>'.
	'<li>Ikke fullført nødprosedyre.</li>'.
	'<li>Cutaway under to åpne skjermer &mdash; med mindre disse flyr "downplane".</li>'.
	'<li>Berget av nødåpner på grunn av manglende trekk.</li>'.
	'<li>Panikkartet/ukontrollert atferd i flyet.</li>'.
	'<li>Ukontrollert skjermkjøring.</li>'.
	'</ul>'.
	'<p>Foranstående betraktes som klare kriterier for at Hoppmester gir karakter "FU" ved bedømmelse av hoppet. Eleven skal gjøres kjent med kjennelsen, og den videre saksbehandling. HM skal, i samråd med Hoppleder, pålegge eleven ytterligere trening med instruktør før hopping kan fortsette. Eleven skal vurderes tilbakeført i progresjonen.</p>'.
	'<p>Hendelsesrapportskjema fylles ut, med nøyaktig angivelse av hendelsesforløp og årsak til kjennelsen. Dersom HM finner at også tidligere hopp er karakterisert som FU, eller hvis FU-kjennelsen er gitt på grunn av manglende trekk, slik at eleven ble berget av nødåpneren, skal eleven gis midlertidig hoppforbud inntil Hovedinstruktøren har gitt sin totalvurdering.</p>'.
	'<p>Hovedinstruktøren vurderer om spesielle treningsformer, og/eller særlig oppfølging skal iverksettes, slik at eleven oppnår de nødvendige ferdigheter som kreves for sikkerhetsmessig forsvarlig fortsettelse av hoppingen. Dersom dette ikke ansees mulig eller forsvarlig skal eleven nektes all videre hopping. Eventuelt vedtak om permanent hoppforbud fattes av Hovedinstruktør. Vedtak føres i elevens hopplogg, ved det hoppet som førte til avgjørelsen, og på den siden som inneholder hopperens personalia, navn og foto. Avgjørelse rapporteres til SU på hendelsesrapportskjema.</p>'
	),

);

?>

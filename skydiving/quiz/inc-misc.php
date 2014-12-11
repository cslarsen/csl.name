<?

function getvar($name, $default)
{
	if ( isset($_POST[$name]) ) return $_POST[$name];
	if ( isset($_GET[$name]) ) return $_GET[$name];
	return $default;
}

?>

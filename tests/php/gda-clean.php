<?php
/* Configure session cache */
session_cache_limiter('nocache');
session_start();

include_once "gda-utils.php";
include_once "gda-config.php";

header('Content-type: text/plain; charset=UTF-8');

$cmdfile = get_command_filename (session_id ());
$replyfile = get_reply_filename (session_id ());

@unlink ($cmdfile);
@unlink ($replyfile);

/* all cleaned */
$reply = new SimpleXMLElement("<reply></reply>");
$reply->addChild ("status", "OK");
echo gda_add_hash ($init_shared, $reply->asXml());
session_destroy ();

?>

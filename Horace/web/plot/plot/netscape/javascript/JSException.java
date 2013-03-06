package netscape.javascript;
/**
 * This class is for convenience when compiling applets.
 * @link http://home.netscape.com/eng/mozilla/3.0/handbook/plugins/doc/netscape.javascript.JSException.html
 *
 * @author Robert Fuller rfuller@applepiesolutions.com
 * @copy Copyright(c) Applepie Solutions Ltd., 2001
 */

public class JSException extends Exception{
    public JSException(){
	    throw new RuntimeException
		("CLASSPATH has the wrong netscape.javascript stuff in it");
    }
    public JSException(String s){
	throw new RuntimeException
	    ("CLASSPATH has the wrong netscape.javascript stuff in it");
	
    }
    public JSException(String s,
		       String filename,
		       int lineno,
		       String source,
		       int tokenIndex){
	throw new RuntimeException
	    ("CLASSPATH has the wrong netscape.javascript stuff in it");
    }
}


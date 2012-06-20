package horaceplot.drs;

import java.io.*;
import java.applet.Applet;

/**
 *
 * Enable communication with the javascript in the browser.
 * 
 * @author dave.tiddeman
 */

public class jscriptstream
{
    String form,name;
    int length;
    netscape.javascript.JSObject obj;
    ByteArrayInputStream bais;
    ByteArrayOutputStream baos;
    
    jscriptstream(String jname,Applet app) throws Exception{
        int i=jname.indexOf(".");
        if(i>0){
          form=jname.substring(0,i);
          name=jname.substring(i+1);
        }
        baos=null;
        bais=null;
        if(!getobject(app)) throw new Exception("Failed to locate object "+jname);
    }
    
    public boolean getobject(Applet app){
        boolean succeeded=false;
	    try{
		netscape.javascript.JSObject win=netscape.javascript.JSObject.getWindow(app);
		netscape.javascript.JSObject document=(netscape.javascript.JSObject)win.getMember("document");
		netscape.javascript.JSObject form1=(netscape.javascript.JSObject)document.getMember(form);
		obj=(netscape.javascript.JSObject)form1.getMember(name);
	    String value=(String)obj.getMember("value");
	    length=value.length();
	    bais=new ByteArrayInputStream(value.getBytes());
		succeeded=true;
		}catch(Exception e){
		    System.out.println(e);
		    }
		return succeeded;
	}
	
	public void flush(){
	    String value=baos.toString();
	    obj.setMember("value",value);
	    baos=null;
	}
	
	public byte read(){
	    if(bais==null){
	        baos=null;
	        String value=(String)obj.getMember("value");
	        length=value.length();
	        bais=new ByteArrayInputStream(value.getBytes());
	    }
	    return (byte)bais.read();
	      
	}
	
	public void write(byte b){
	    if(baos==null){
	        bais=null;
	        baos=new ByteArrayOutputStream();
	    }
	    baos.write(b);
	}
    
    public void writeint(int q[]){
        for(int i=0;i<q.length;i++){
            writeint(q[i]);
        }
    }

    public void writeint(int q){
        writeshort((short)(q&65535));
        writeshort((short)(q>>16));
    }
    
    public int readint(){
        int q=(int)readshort();
        if(q<0)q+=65536;
        q=q|(((int)readshort())<<16);
        return q;
    }
    
    public void writeshort(short q[]){
        for(int i=0;i<q.length;i++){
            writeshort(q[i]);
        }
    }
    
    public void writeshort(short q){
           write((byte)((q&63)+32));
           write((byte)(((q&1984)>>6)|32));
           write((byte)(((q&63488)>>11)|32));
    }
            
    
    public short readshort(){
            int q1=read();
            int q=((q1-32)&63);
            q1=read();
            q=q|((q1&31)<<6);
            q1=read();
            q=q|((q1&31)<<11);
            return (short)q;
    }
        
    
    
    
}

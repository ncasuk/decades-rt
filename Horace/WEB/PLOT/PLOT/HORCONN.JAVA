
import java.io.*;
import java.net.*;
import java.applet.*;

/**
 * Sets up a connection to a server on Horace.
 * 
 * @author Dave Tiddeman
 * @version 1
 */


public class horconn {
public Socket conn;
private InputStream is;
private OutputStream os;
private BufferedOutputStream bos;
public DataInputStream reader;
public DataOutputStream writer;
private boolean b;
private String host;
private int port;

   /**
    * Set up connection to Horace
    * 
    * @param hos Host name
    * @param por Port number
    * @exception java.io.IOException Failed connection
    */

   horconn(String hos,int por) throws IOException{
      host=hos;
      port=por;
      connect();
   }

   /**
    * Set up connection to horace
    * 
    * @param app Applet
    * @param por1 Alternative port number
    * @exception java.io.IOException Failed connection
    */
    
   horconn(Applet app,int por1) throws IOException{
      host=app.getParameter("host");
      if(host==null){
        host=app.getCodeBase().getHost();
        if(host.length()==0)host="horace";
      }
      if(por1<0){
      String por=app.getParameter("port");
      if(por!=null){
        port=Integer.parseInt(por);
      }else{
        port=1500;
      }
      }else{
        port=por1;
      }
      System.out.println("Connecting to host> "+host+":"+port);
      connect();
   }
   /**
    * Set up connection to horace
    * 
    * @param app Applet
    * @exception java.io.IOException Failed connection
    */
      
    horconn(Applet app) throws IOException{
      this(app,-1);
    }

    /**
     * Set up connection to horace
     */
    horconn(){
    }

   /**
    * Make connection
    * 
    * @exception java.io.IOException Failed connection
    */
   private void connect() throws IOException{   
      conn = new Socket(host, port);
      conn.setSoTimeout(0);
      is = conn.getInputStream();
      os = conn.getOutputStream();
      reader = new DataInputStream(is);
      bos=new BufferedOutputStream(os,256);
      writer = new DataOutputStream(bos);
   }

   /**
    * Write string to Horace
    * 
    * @param st String
    * @exception java.io.IOException Connection dropped
    */

   
   public void writeString(String st) throws IOException{
    writer.writeBytes(st);
   }
   
   /**
    * Write a character to HORACE
    * 
    * @param c Character
    * @exception java.io.IOException Connection dropped
    */
   public void writechar(char c) throws IOException{
    writer.writeByte((byte)c);
    writer.flush();
   }
   
   /**
    * Read int from HORACE
    * 
    * @return int sent by Horace
    * @exception java.io.IOException Connection dropped
    */
   
   public int readint() throws IOException{
     return reader.readInt();
   }
   
   /**
    * Read short from HORACE
    * 
    * @return short sent by Horace
    * @exception java.io.IOException Connection dropped
    */
   public short readshort() throws IOException{
     return reader.readShort();
   }
   
   /**
    * Read string from Horace
    * 
    * @return String sent by Horace
    * @exception java.io.IOException Connection dropped
    */

   
   public String readLine() throws IOException{
    String st=new String();
    char c=0;
    while(c!=(char)10){
        c=(char)reader.readByte();
        st+=c;
    }
    return st;
   }

   /**
    * Read string from Horace
    * 
    * @param n Length of String
    * @return String
    * @exception java.io.IOException Connection dropped
    */
   public String readString(int n) throws IOException{
    String st=new String();
    for(int i=0;i<n;i++){
        st+=(char)reader.readByte();
    }
    return st;
   }
   
   /**
    * Reads characters from horace
    * 
    * @param n Number of characters
    * @return Array of characters
    * @exception java.io.IOException Connection dropped
    */
   public char[] readchars(int n) throws IOException{
    char[] st=new char[n];
    for(int i=0;i<n;i++){
        st[i]=(char)reader.readByte();
    }
    return st;
   }
   

   /**
    * Reads one character from horace
    * 
    * @return character
    * @exception java.io.IOException Connection dropped
    */
   public char readchar() throws IOException{
    return (char)reader.readByte();
   }

   /**
    * Write integer to Horace
    * 
    * @param i Integer
    * @exception java.io.IOException Connection dropped
    */
   
   public void writeint(int i) throws IOException{
    writer.writeInt(i);
//    writer.flush();
   }

   /**
    * Flush output
    * 
    * @exception java.io.IOException Connection dropped
    */
   
  
   public void flush() throws IOException{
    writer.flush();
   }

     


   /**
    * Close connection
    * 
    */
    
   public void close(){
    synchronized(this){
        try{
       is.close();
       os.close();
       conn.close();
        }catch(IOException ioe){
            System.out.println(ioe);}
//       conn=null;
    }
   }

   
}

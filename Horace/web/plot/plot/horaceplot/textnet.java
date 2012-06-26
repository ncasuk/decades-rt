package horaceplot;

import java.awt.*;
import java.applet.*;
import java.io.*;
/**
 * In conjunction with textnet.html creates 
 * a text based connection to horace
 * 
 * @author dave.tiddeman
 */

public class textnet extends Applet implements Runnable
{
    Thread t;
    boolean keeprunning;
    public int cols=80;
    public int rows=25;
    public int totalrows=25;
    public int xm,ym;
    public int cursorx=0,cursory=0;
    public Color foreg,backg;
    public int fontsize;
    public Font fnt;
    public horconn HC;
    public int changes;
    public StringBuffer text;
    public String blankline;
    public boolean Edit=false;
    public String hostname="192.168.101.71";
    public int portno=1500;
    symk asymk;
    
	public void init()
	{
		// This code is automatically generated by Visual Cafe when you add
		// components to the visual environment. It instantiates and initializes
		// the components. To modify the code, only use code syntax that matches
		// what Visual Cafe can generate, or Visual Cafe may be unable to back
		// parse your Java file into its visual environment.
		//{{INIT_CONTROLS
		setLayout(null);

		setBackground(java.awt.Color.white);
		setForeground(java.awt.Color.black);
		setSize(600,500);
		//}}
	
		//{{REGISTER_LISTENERS
		//}}
        String host=getParameter("host");
        if(host!=null){
            hostname=host;
        }else{
            try{
                hostname=this.getCodeBase().getHost();
            }catch(Exception e){}
        }
        String port=getParameter("port");
        if(port!=null)portno=Integer.parseInt(port);
		String disptype=getParameter("Display");
		StringBuffer sb=new StringBuffer();
		for(int x=0;x<cols;x++){
                sb.append(" ");
        }
        sb.append('\n');
        blankline=sb.toString();
		newdisplay(disptype);
		asymk = new symk();
		this.addKeyListener(asymk);
	}
	

	public void newdisplay(String disptype){
		int disp=1;
		if(disptype!=null){
		 if(disptype.equals("NONE"))disp=0;
		 if(disptype.equals("SATCOM"))disp=1;
		 if(disptype.equals("GPS"))disp=2;
		 if(disptype.equals("NDU"))disp=3;
		 if(disptype.equals("NEVZ"))disp=4;
		 if(disptype.equals("INU"))disp=5;
		 if(disptype.equals("STATUS"))disp=6;
		 if(disptype.equals("DRS"))disp=7;
		 if(disptype.equals("NEPH"))disp=8;
		}
        cls();
        if(disp!=0){
        try{
          HC=new horconn(hostname,portno);
          HC.writeString("TEXT");
          HC.writechar((char)disp);
        }catch(Exception e){}
		changes=0;
		t=new Thread(this);
		t.start();
		requestfocus();
		}
    }
	  
	public void changedisplay(String disp){
	    stopit();
	    newdisplay(disp);
	}
	
	//{{DECLARE_CONTROLS
	//}}

    public void cls(){
//        System.out.println("CLS");
        StringBuffer sb=new StringBuffer();
        for(int y=0;y<rows;y++){
            sb.append(blankline);
        }
        text=sb;
        totalrows=rows;
        changes++;
    }
    
    public void write(char c){
        if(cursorx>=cols){
//            System.out.println("line too long");
            cr();
            lf();            
        }
        if(cursory>=rows){
            int xtra=cursory-rows+1;
            totalrows+=xtra;
            cursory=rows-1;
            for(int i=0;i<xtra;i++)text.append(blankline);
        }
        int pos=((totalrows-rows)+cursory)*(cols+1)+cursorx;
        text.setCharAt(pos,c);
        changes++;
        cursorx++;
    }
    
    public void write(char[] c){
        for(int i=0;i<c.length;i++){
            write(c[i]);
        }
    }
    public void cr(){
//        System.out.println("CR");
        cursorx=0;
    }
    public void lf(){
//        System.out.println("LF");
        cursory++;
    }
    public void ff(){
//        System.out.println("FF");
        cursorx=0;
        cursory=0;
    }
    public void tab(){
//        System.out.println("tab");
        cursorx+=5;
        if(cursorx>=cols)cursorx=cols-1;
    }
    
    public void vtab(){
//        System.out.println("vtab");
        cursory+=5;
        if(cursory>=rows)cursory=rows-1;
    }
    
    public void del(){
        cursorx--;
        if(cursorx<0){
            cursory--;
            if(cursory>=0){
                cursorx=cols-1;
            }else{
                cursory=0;
                cursorx=0;
            }
        }
        int pos=((totalrows-rows)+cursory)*(cols+1)+cursorx;
        text.setCharAt(pos,(char)32);
       
        changes++;
    }
    
    public void write(char[] c,int s,int l){
        for(int i=s;i<l;i++){
            write(c[i]);
        }
    }
        
    public void position(int x,int y){
//        System.out.println("Position "+x+","+y);
        if((x>=0)&&(y>=0)&&(x<cols)&&(y<rows)){
        cursorx=x;
        cursory=y;
        }
    }
    
    public void run()
    {   
        while(HC!=null){
      if(!Edit){
        try{
        char s=HC.readchar();
        if((byte)s<0)s=(char)(256+s);
            switch(s){
            case (char)9:tab();break;
            case (char)10:cr();break;
            case (char)11:vtab();break;
            case (char)12:ff();break;
            case (char)13:lf();break;
            case (char)127:del();
                           break;
            case (char)27:
              char s2=HC.readchar();
              if(s2=='['){                
                String s3=HC.readString(2);
//                System.out.print("["+s3);
                if((s3.equals("2J"))||(s3.equals("0m"))
                   ||(s3.equals("7m"))){
                    if(s3.equals("2J")){
                      cls();
                    }
                    if(s3.equals("0m")){
//                      hlight=true;
                    }
                    if(s3.equals("7m")){
//                        hlight=false;
                    }
                }else{
                      while(s3.charAt(s3.length()-1)!='H'){
                        s3+=HC.readString(1);
                      }
                      s3=s3.substring(0,s3.length()-1);
                      int i1=s3.indexOf(";");
                      int x=Integer.parseInt(s3.substring(0,i1));
                      int y=Integer.parseInt(s3.substring(i1+1));
                      position(y-1,x-1);
                }
              }else{
                if(s2=='E'){
                    Edit=true;
                    this.removeKeyListener(asymk);
                }
              }
              break;
            
            default: 
                     if(((byte)s<32)||((byte)s>125))
                       System.out.println((byte)s);
            
                     write(s);
             }
        }catch(IOException e){
            System.out.println(e);
            HC.close();
            HC=null;
            System.out.println("Lost connection");
//            this.removeKeyListener(asymk);

            }
         }else{
            try{
                Thread.sleep(200);
            }catch(Exception e){}
         }
        }
        text=new StringBuffer("\n\n\n     Lost Connection ");
        changes++;
    }
    

	class symk extends java.awt.event.KeyAdapter
	{
		public void keyPressed(java.awt.event.KeyEvent event)
		{
			Object object = event.getSource();
//			if (object == texttext1)
				texttext1_KeyPressed(event);
		}

		public void keyTyped(java.awt.event.KeyEvent event)
		{
			Object object = event.getSource();
//			if (object == texttext1)
				texttext1_KeyTyped(event);
		}
	}

	public void texttext1_KeyTyped(java.awt.event.KeyEvent event)
	{
		// to do: code goes here.
		char c=event.getKeyChar();
		if(c==(char)10)c=(char)13;
        if(c==(char)8){
            c=(char)127;
        }
		System.out.println(c+" "+(byte)c);
        if(HC!=null){
		try{HC.writechar(c);
		}catch(Exception e){
		    System.out.println(e);}
		}
	}

	public void texttext1_KeyPressed(java.awt.event.KeyEvent event)
	{
		// to do: code goes here.
//		System.out.println("Hello");
			 
	}
  
    public void stopit(){
        if(HC!=null)HC.close();
//        if(t!=null)t.stop();
        t=null;
    }
    

    public void stop(){
        stopit();
        super.stop();
    }
    
	public String getText(){
	    changes=0;
	    StringBuffer sb=new StringBuffer(text.toString());
	    if(!Edit){
         int pos=((totalrows-rows)+cursory)*(cols+1)+cursorx;
         if(pos<sb.length()){
           if(sb.charAt(pos)==(char)32){
             sb.setCharAt(pos,'>');
           }
         }
	    }
	    return sb.toString();
	}
	
	public void upload(String s){
	    try{HC.writeString(s);
	        HC.writechar((char)26);
	    }catch(Exception e){}
	    Edit=false;
	    this.addKeyListener(asymk);
	    this.requestFocus();
	}
	
	public void upwrite(String s){
	    try{HC.writeString(s);
	        HC.flush();
	    }catch(Exception e){}
	}
	
	public void requestfocus(){
	    this.requestFocus();
	}
}
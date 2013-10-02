package horaceplot.choices;


import java.awt.*;
import java.applet.*;
import java.net.*;
import horaceplot.stringutl;

/**
 * Applet form for choosing parameters to plot
 * 
 * @author Dave Tiddeman
 * @version 1.1
 */

public class choices extends Applet
{

    /**
     * Parameter List
     * 
     * @see paralist
     */
    private static final long serialVersionUID = 4517345;
    public paralist pl;
    public int minpara=1;
    public int maxpara,pni;
    public int one=1;
    public int[] paranum,paracol,parasym;
    public String[] paranam;
    boolean dolines=true;
    public final int maxp=50;
    String[] cols={"White","Black","Red","Green","Blue","Cyan",
                 "Orange","Magenta","Pink","Yellow","Gray"};
    String[] syms={"Line","Dot","Cross","X",
                   "Square","Diamond"};
	java.awt.Choice indchoice;
	java.awt.List depchoice;
	java.awt.Choice[] col=new Choice[maxp];
	java.awt.Choice[] sym=new Choice[maxp];
	java.awt.Label[] label=new Label[maxp+1];
	java.awt.Button[] delbutton=new Button[maxp];
//	java.awt.Button addbutton;
	java.awt.Color backgr=new Color(24,111,77);
	java.awt.Color foregr=new Color(228,242,59);

	/**
	 * Read in Parameter info and make form
	 */
    
	public void init()
	{
		setLayout(null);
    	setSize(550,500);
		setBackground(backgr);
	
    String paras=getParameter("paras");
    String colours=getParameter("colours");
    String symbols=getParameter("symbols");
    String names=getParameter("names");
    pni=0;
	String filename=getParameter("filename");
	if(filename==null) filename=new String("Parano.txt");
	try{
	    URL textfile=new URL(getCodeBase(),filename);
	    pl=new paralist(textfile,0,4,13,24,25,54,"***");
	    System.out.print("reset(["+names+"]["+paras);
        System.out.println("]["+colours+"]["+symbols+"])");
        reset(names,paras,colours,symbols,dolines);
	}catch(MalformedURLException mue){
	    System.out.println("URL bad");
	}
	}
	
    /**
     * Get the index of parameter in list
     * 
     * @param i index of parameter
     * @return Index of selected parameter 
     */
    public int getInd(int i){
        int ans;
        System.out.println("Ind="+i);
        if((i==0)&&dolines){
            ans=indchoice.getSelectedIndex();
        }else{
            ans=depchoice.getSelectedIndexes()[i-one];
        }
        return ans;
    }
    /**
     * Get the name of parameter
     * @param i Index of parameter
     * @return Name of selected parameter(i)
     */
    public String getItem(int i){
        System.out.println("Item="+i);
        String ans;
        if((i==0)&&dolines){
            ans=indchoice.getSelectedItem();
        }else{
            ans=depchoice.getSelectedItems()[i-one];
        }
        return ans;
    }
    /**
     * Sets/unsets parameter
     * @param i Index of parameter
     * @param tf true to select false to deselect
     */
    public void setSel(int i,boolean tf){
        if(i==0){
            indchoice.select(i);
        }else{
            if(tf){
                depchoice.select(i);
            }else{
                depchoice.deselect(i);
            }
        }
    }
        
     /**
      * Get name of parameter from list
      * @param i index of list
      * @return the name
      */
     
    public String getName(int i){
        String Name;
        Name=pl.getName(getInd(i));
        return Name;
    }

    /**
     * Get the number of parameter
     * 
     * @param i Index of parameter
     * @return Number of parameter (i)
     */
    public int getNumber(int i){
        int Number=pl.getNumber(getInd(i));
        return Number;
    }

    /**
     * Get the units of parameter
     * 
     * @param i Index of parameter
     * @return Units of parameter (i)
     */
    public String getUnits(int i){
        String Units=pl.getUnits(getInd(i));
        return Units;
    }
    /**
     * Get the full name of parameter
     * @param i index of parameter
     * @return name of parameter
     */
    public String getWholename(int i){
        String ans= getItem(i);
        return ans;
    }

    /**
     * Sets a particular parameter as selected
     * 
     * @param defpara Parameter number
     * @param i Index of parameter
     */
    public void setSelected(int defpara,int i){
	  int ip=pl.getIndex(defpara);
	  setSel(ip,true);
    }

    /**
     * Get the number of parameter chosen
     * 
     * @return Number of parameters to plot
     */
    
    public int getNumpara(){
           System.out.println("getNumpara "+(pni+one));
           return (pni+one);
    }
    /**
     * Put the colours chosen into string
     * @return String of colour indexes
     */
    public String getcolours(){
        System.out.println("getcolours");
        StringBuffer ps=new StringBuffer();
        for(int i=0;i<pni;i++){
            ps.append(col[i].getSelectedIndex());
            if(i!=(pni-1)){ps.append(",");}
        }
        return ps.toString();
    }
    /**
     * Put the symbols chosen into string
     * @return String of symbol indexes
     */
    public String getsymbols(){
        System.out.println("getsymbols");
        StringBuffer ps=new StringBuffer();
        for(int i=0;i<pni;i++){
            ps.append(sym[i].getSelectedIndex()-1);
            if(i!=(pni-1)){ps.append(",");}
        }
        return ps.toString();
    }
	/**
	 * puts the parameters numbers into a string
	 * @return string of comma seperated parameters numbers
	 */
    
    public String getparas(){
        System.out.println("getparas");
        StringBuffer ps=new StringBuffer();
        for(int i=0;i<(pni+one);i++){
            ps.append(pl.getNumber(getInd(i)));
            if(i!=(pni-1+one)){ps.append(",");}
        }
        return ps.toString();
    }
	/**
	 * puts the parameters names into a string
	 * @return string of comma seperated parameters names
	 */
    
    public String getparans(){
        System.out.println("getparans");
        StringBuffer ps=new StringBuffer();
        for(int i=0;i<(pni+one);i++){
            ps.append(paranam[i]);
            if(i!=(pni-1+one)){ps.append(",");}
        }
        return ps.toString();
    }
	/**
	 * puts the parameters names into a string
	 * @return string of comma seperated parameters names
	 */
    public String getnames(){
        System.out.println("getnames");
        StringBuffer ps=new StringBuffer();
        for(int i=0;i<(pni+one);i++){
            ps.append(pl.getName(getInd(i)));
            if(i!=(pni-1+one)){ps.append(",");}
        }
        return ps.toString();
    }
	/**
	 * puts the parameters units into a string
	 * @return string of comma seperated parameters units
	 */
    public String getunits(){
        System.out.println("getunits");
        StringBuffer ps=new StringBuffer();
        for(int i=0;i<(pni+one);i++){
            ps.append(pl.getUnits(getInd(i)));
            if(i!=(pni-1+one)){ps.append(",");}
        }
        return ps.toString();
    }

    /**
     * Make the form
     */
    
    public void makechoice(){
        if(indchoice==null){
            indchoice=new Choice();
            depchoice=new List();
            pl.makechoice(indchoice);
            pl.makelist(depchoice);
            add(indchoice);
            add(depchoice);
            depchoice.setMultipleMode(true);
            indchoice.setBounds(48,20,300,25);
            depchoice.setBounds(48,50,300,100);
        }else{
		    int[] xx=depchoice.getSelectedIndexes();
            for(int i=0;i<xx.length;i++){
                depchoice.deselect(xx[i]);
            }
        }
        indchoice.setVisible(dolines);
        indchoice.setEnabled(dolines);
        slistitem q111=new slistitem();
        depchoice.addItemListener(q111);
        indchoice.select(pl.getIndex(paranum[0]));
	    if(label[0]==null){
	        label[0]=new Label();
	        add(label[0]);
	        label[0].setForeground(foregr);
	        label[0].setBounds(8,20,40,25);
	      }
	      if(dolines){
	        label[0].setText(paranam[0]);
	      }else{
	        label[0].setText("");
	      }
    	for(int i=0;i<maxpara;i++){
//	          col[i].select(paracol[i]);
//	          sym[i].select(parasym[i]+1);

	      int ip=pl.getIndex(paranum[i+1]);
	          col[i]=new java.awt.Choice();
	          sym[i]=new java.awt.Choice();
	      if(dolines){
	          for(int ii=0;ii<cols.length;ii++){
	            col[i].add(cols[ii]);
	          }
	          add(col[i]);
	          col[i].setBounds(350,170+i*25,75,25);
	          for(int ii=0;ii<syms.length;ii++){
	            sym[i].add(syms[ii]);
	          }
	          add(sym[i]);
	          sym[i].setBounds(430,170+i*25,80,25);
	          col[i].select(paracol[i]);
	          sym[i].select(parasym[i]+1);
	      }
	      if(label[i+1]==null){
	        label[i+1]=new Label();
	        add(label[i+1]);
	        label[i+1].setForeground(foregr);
	        label[i+1].setBounds(8,170+i*25,340,25);
	      }
	      label[i+1].setText(paranam[i+one]+" "+pl.toString(pl.getIndex(paranum[i+one])));
//          if((i>=minpara)){
	        if(delbutton[i]==null){
	          delbutton[i]=new Button();
	          add(delbutton[i]);
	          delbutton[i].setBounds(510,170+i*25,30,25);
	          delbutton[i].setLabel("Del");
	          delbutt Delbutt=new delbutt();
    	      delbutton[i].addActionListener(Delbutt);
  	          Delbutt.index=i;
    	    }
//	        if(i>=pni){
//              delbutton[i].setEnabled(false);
//              delbutton[i].setVisible(false);
//            }else{
//              delbutton[i].setEnabled(true);
//              delbutton[i].setVisible(true);
//            }   
//	      }
	      if(i>=pni){
	        enab(i,false);
          }else{
            enab(i,true);
	        depchoice.select(ip);
          }
	    }
	}

/**
 * Class to deal with pressing Del button
 * 
 */


public class delbutt implements java.awt.event.ActionListener
{
    int index;

    /**
     * Del button pressed
     * 
     * @param e Event
     */
    public void actionPerformed(java.awt.event.ActionEvent e)
    {
        // This method is derived from interface java.awt.event.ActionListener
        // to do: code goes here
        int[] xx=depchoice.getSelectedIndexes();
        System.out.println(xx.length);
        if(xx.length>0){
          depchoice.deselect(xx[index]);
          synchrolists();
        }
       
    }

}    

  /**
   * Reset the list
   * @param names names for plot axis 
   * @param paras parameter numbers
   * @param colours colour indexes
   * @param symbols symbols
   * @param dol line choices ( yes or no )
   */

    public void reset(String names,String paras,
            String colours,String symbols,boolean dol){
      if(paras==null)paras="515,520";
      if(names==null)names="X,Y1,Y2,Y3,Y4,Y5";
      if(colours==null)colours="1,2,3,4,5";
      if(symbols==null)symbols="0,0,0,0,0";
      dolines=(dol==true);
	  one=dolines?1:0;
      for(int i=0;i<maxpara;i++){
 	    label[i+1].setText("");       
        enab(i,false);
 //       if(delbutton[i]!=null){
 //           delbutton[i].setVisible(false);
 //       }
      }
      pni=0;
      int pnu=0;
      int pna=0;
//      paranum=new int[maxp];
//      paracol=new int[maxp];
//      parasym=new int[maxp];
//      paranam=new String[maxp];
      paranum=stringutl.stringsepi(paras,",",maxp);
      pni=stringutl.countseps(paras,",")-one;
      maxpara=0;
      paranam=stringutl.stringsep(names,",",0);
      maxpara=paranam.length-one;
      parasym=stringutl.stringsepi(symbols,",",maxpara);
      paracol=stringutl.stringsepi(colours,",",maxpara);
      makechoice();
      synchrolists();
    }
/**
 * Enable or disable 
 * @param i index to affect
 * @param tf enable or disable
 */
    public void enab(int i,boolean tf){
 //       label[i+1].setVisible(tf);
         delbutton[i].setEnabled(tf);
         delbutton[i].setVisible(tf);
         if(!dolines)tf=false;
         col[i].setEnabled(tf);
         col[i].setVisible(tf);
         sym[i].setEnabled(tf);
         sym[i].setVisible(tf);
    }
    
    /**
     * Class to deal with list items
     * @author dave.tiddeman
     *
     */
	class slistitem implements java.awt.event.ItemListener
	{
	    boolean changeable=true;
/**
 *  If something has happened
 */	    
		public void itemStateChanged(java.awt.event.ItemEvent event)
		{
		int[] xx=depchoice.getSelectedIndexes();
		int indq=((Integer)event.getItem()).intValue();
		if(xx.length>maxpara)depchoice.deselect(indq);
		xx=depchoice.getSelectedIndexes();
		for(int i=maxpara;i<xx.length;i++){
		  depchoice.deselect(xx[i]);
		}
//		if(xx.length<minpara){
//		   depchoice.select(indq);
//		}
		synchrolists();
		}
	}
	
/**
 * Synchronize the list to how it should be.
 *
 */
void synchrolists (){
		int[] xx=depchoice.getSelectedIndexes();
//		if(xx.length>=minpara){
		  pni=xx.length;
		for(int i=0;i<maxpara;i++){
		  if(i<pni){
		         label[i+1].setText(paranam[i+one]+" "+pl.toString(xx[i]));
		         enab(i,true);
//		         if(i>=minpara){
//		            delbutton[i].setVisible(true);
//		            delbutton[i].setEnabled(true);
//		         }
		  }else{
		    label[i+1].setText(paranam[i+one]);
		    enab(i,false);
//		    delbutton[i].setVisible(false);
//		    delbutton[i].setEnabled(false);
		    
		  }
		 }
//		}
}


}

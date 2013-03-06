package horaceplot;

import java.awt.*;
import java.applet.*;
import java.net.*;
import java.io.*;
/**
 * More accurate time through UDP packets, and ability to 
 * use the event mark
 * 
 * 
 * @author dave.tiddeman
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class TimeEvents extends Applet implements Runnable,java.awt.event.MouseListener
{
    DatagramSocket socket;
    DatagramPacket InPacket,OutPacket;
    byte[] InMessage = new byte [256];
    byte[] OutMessage = {69};
    int    port=1501;
    String received;
    InetAddress hostadd,address;
    boolean running=false;
    
	public void init()
	{
		setLayout(null);
		String col=getParameter("bgcolor");
		if(col!=null)setBackground(Color.decode(col));
		col=getParameter("fgcolor");
		if(col!=null)setForeground(Color.decode(col));
		col=getParameter("font");
		if(col!=null)setFont(new Font(col, 0, 10));
		add(DatagramLabel);
		DatagramLabel.setBackground(getBackground());
		DatagramLabel.setForeground(getForeground());
		DatagramLabel.setFont(getFont());
		DatagramLabel.setBounds(0,0,400,30);
		DatagramLabel.setText("Check Security settings");
		EButton.setBackground(getBackground());
		EButton.setForeground(getForeground());
		EButton.setFont(getFont());
		EButton.setLabel("Event mark");
		add(EButton);
		EButton.setBounds(420,0,80,30);
        try {
            socket = new DatagramSocket(port);
        } catch (Exception e) {
            System.out.println("Couldn't create new DatagramSocket");
            return;
        }
        System.out.println("Created socket");
        System.out.println(address);
		try{
		    InPacket=new DatagramPacket(InMessage,InMessage.length,address,port);
		    getDatagram();
		    address=InPacket.getAddress();
			System.out.println(address);
		    int port1=InPacket.getPort();
		    OutPacket=new DatagramPacket(OutMessage,OutMessage.length,address,port1);
		}catch(Exception se)
		{System.out.println(se);}

		EButton.addMouseListener(this);
		running=true;
		new Thread(this).start();
		
	}
	
	java.awt.Label DatagramLabel = new java.awt.Label();
	java.awt.Button EButton = new java.awt.Button();
	
	public void stop(){
	    System.out.println("Close socket");
	    running=false;
	    socket.close();
	}
	
    public void run()
    {
        while(running){
              getDatagram();
              }
    }

   public void getDatagram(){
            try{
            socket.receive(InPacket);
            received= new String(InPacket.getData());
            received=received.substring(0,InPacket.getLength()-1);
             DatagramLabel.setText(received);
            }catch(IOException ioe){
                System.out.println("IO error : "+ioe);
            }
   }

		public void mouseClicked(java.awt.event.MouseEvent event)
		{
			Object object = event.getSource();
			if (object == EButton)
		try{
		socket.send(OutPacket);
            }catch(IOException ioe){
                System.out.println("IO error : "+ioe);
            }
		}
		public void mousePressed(java.awt.event.MouseEvent event)
		{
		}
		public void mouseReleased(java.awt.event.MouseEvent event)
		{
		}
		public void mouseEntered(java.awt.event.MouseEvent event)
		{
		}
		public void mouseExited(java.awt.event.MouseEvent event)
		{
		}

}

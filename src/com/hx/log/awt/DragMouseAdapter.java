/**
 * file name : DragMouseListener.java
 * created at : 9:29:40 PM Nov 24, 2015
 * created by 970655147
 */

package com.hx.log.awt;

import java.awt.Component;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

// ��ק�����MouseListener
public class DragMouseAdapter extends MouseAdapter {

	// ���������, �Լ���קǰ��x, y
	protected Component component;
	private int lastX = -1;
	private int lastY = -1;
	
	// ��ʼ��
	public DragMouseAdapter(Component component) {
		super();
		this.component = component;
	}
	
	// ��¼��ק֮ǰ��λ��
	@Override
	public void mousePressed(MouseEvent e) {
		lastX = e.getX();
		lastY = e.getY();
	}
	
	// ��ק��ʱ��, ִ��ҵ���߼�, ����λ��
	@Override
	public void mouseDragged(MouseEvent e) {
		if(lastX != -1) {
			java.awt.Point point = component.getLocation();
			point.x += e.getX() - lastX;
			point.y += e.getY() - lastY;
			component.setLocation(point );
		}
	}
	
}

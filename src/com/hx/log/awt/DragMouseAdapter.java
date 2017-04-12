/**
 * file name : DragMouseListener.java
 * created at : 9:29:40 PM Nov 24, 2015
 * created by 970655147
 */

package com.hx.log.awt;

import java.awt.Component;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

// 拖拽组件的MouseListener
public class DragMouseAdapter extends MouseAdapter {

	// 依赖的组件, 以及拖拽前的x, y
	protected Component component;
	private int lastX = -1;
	private int lastY = -1;
	
	// 初始化
	public DragMouseAdapter(Component component) {
		super();
		this.component = component;
	}
	
	// 记录拖拽之前的位置
	@Override
	public void mousePressed(MouseEvent e) {
		lastX = e.getX();
		lastY = e.getY();
	}
	
	// 拖拽的时候, 执行业务逻辑, 更新位置
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

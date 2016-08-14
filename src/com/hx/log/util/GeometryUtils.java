/**
 * file name : GeometryUtil.java
 * created at : 2:12:59 PM Nov 26, 2015
 * created by 970655147
 */

package com.hx.log.util;

import java.awt.geom.Point2D;

// ������ؼ���
public class GeometryUtils {

	// check
	public static void check(Point2D p) {
		Log.log("dis to 'zero' : " + distanceOfPoints(defaultZero, p) );
	}
	
    // ����p1, p2֮��ľ���
    public static double distanceOfPoints(double x1, double y1, double x2, double y2) {
        double disX = x2 - x1;
        double disY = y2 - y1;
        return Math.sqrt(disX * disX + disY * disY);
    }
    public static double distanceOfPoints(Point2D p1, Point2D p2) {
    	return distanceOfPoints(p1.getX(), p1.getY(), p2.getX(), p2.getY() );
    }

    // ����p1, p2���е�
    public static Point2D middlePoint(double x1, double y1, double x2, double y2) {
        double x = (x1 + x2) / 2;
        double y = (y1 + y2) / 2;
        return new Point2D.Double(x, y);
    }
    public static Point2D middlePoint(Point2D p1, Point2D p2) {
    	return middlePoint(p1.getX(), p1.getY(), p2.getX(), p2.getY() );
    }

    // ����p1 -> p2�����Ͼ���p1ΪdisToStartPoint�ĵ�
    public static Point2D extentPoint(double x1, double y1, double x2, double y2, double disToStartPoint) {
        double dis = distanceOfPoints(x1, y1, x2, y2);
        double sin = (y2 - y1) / dis;
        double cos = (x2 - x1) / dis;
        double deltaX = disToStartPoint * cos;
        double deltaY = disToStartPoint * sin;

        return new Point2D.Double(x1 + deltaX, y1 + deltaY);
    }
    public static Point2D extentPoint(Point2D p1, Point2D p2, double disToStartPoint) {
    	return extentPoint(p1.getX(), p1.getY(), p2.getX(), p2.getY(), disToStartPoint );
    }
    
    // ��p1, p2��p1Ϊ��׼��ʱ�뷴ת90��
    // 					e'->|
    //  -----    =>			|
    // /|\ /|\				|
    //  |	|				|
    //  s	e			s->	|
    public static Point2D verticalPoint(Point2D p1, Point2D p2) {
    	return verticalPoint(p1.getX(), p1.getY(), p2.getX(), p2.getY() );
    }
    public static Point2D verticalPoint(double x1, double y1, double x2, double y2) {
    	return rotate(x1, y1, x2, y2, 90);
    }
    
    // Ĭ�ϵ�ԭ������[ûָ����׼��, Ĭ����Զ��Ϊ��׼��]
    public final static Point2D defaultZero = new Point2D.Double(0, 0);
    
    // ���p1, p2��y������ͬ, ��ֱ����תdegree��
    	// ����, ���������y������p1��y����Ϊ��׼, ��תdegree + '(p1 -> p2)�����x��Ķ���'
    public static Point2D rotate(double x1, double y1, double x2, double y2, double degree) {
//    	Log.log(y1, y2);
//    	Log.log("degree : " + degree);
    	double dis = distanceOfPoints(x1, y1, x2, y2);
    	if(! equals(y1, y2) ) {
    		// ͨ��tan�������		 	(tan) => Math.toDegrees(Math.atan(tan ))
    		return rotate(x1, y1, x1+dis, y1, degree + Math.toDegrees(Math.atan((y2 - y1) / (x2 - x1)) ) );
    	}
    	double radians = Math.toRadians(degree);
    	double sin = Math.sin(radians );
    	double cos = Math.cos(radians );
    	
    	double offX = dis * cos;
    	double offY = dis * sin;
    	
    	return new Point2D.Double(x1 + offX, y1 + offY);
    }
    public static Point2D rotate(Point2D p1, Point2D p2, double degree) {
    	return rotate(p1.getX(), p1.getY(), p2.getX(), p2.getY(), degree);
    }
    
    // ��p1��Զ��Ϊ��׼��תdegree��
    public static Point2D rotate(double x, double y, double degree) {
        return rotate(defaultZero.getX(), defaultZero.getY(), x, y, degree);
    }
    public static Point2D rotate(Point2D p, double degree) {
    	return rotate(p.getX(), p.getY(), degree);
    }
    
    // ������double��Ϊ��ͬ�Ĳ�ֵ����ֵ[���������ж�]
    public final static double minOff = 0.0001d;
    // �ж������������Ƿ���ͬ
    public static boolean equals(double d1, double d2) {
    	return Math.abs(d1 - d2) < minOff;
    }
	
    // ��Point2Dת��ΪPoint
    public static java.awt.Point toPoint(Point2D p2d) {
    	return new java.awt.Point((int) p2d.getX(), (int) p2d.getY() );
    }
    
}

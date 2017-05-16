/**
 * file name : Test09ForGeometryUtils.java
 * created at : 下午4:38:20 2016年8月6日
 * created by 970655147
 */

package com.hx.log.test;

import java.awt.geom.Point2D;

import com.hx.common.math.GeometryUtils;
import com.hx.log.util.Log;

public class Test09ForGeometryUtils {
	
	// com.hx.test
	// refer : http://blog.csdn.net/ycb1689/article/details/7656171
	public static void main(String[] args) {
		Log.log.setLogPattern(com.hx.log.util.Constants.JUST_PRINT_MSG_LOG_PATTERN);
		
//		Point2D p = rotate(50, 0, 30);
		Point2D p = GeometryUtils.rotate(43.30127018922194, 24.999999999999996, 60);
		Log.log(p );
		
		GeometryUtils.check(p );
		p = GeometryUtils.rotate(0, 0, 10, 10, 90);
		
		Log.log(p );
		GeometryUtils.check(p );
		
	}

}

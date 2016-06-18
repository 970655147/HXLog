/**
 * file name : ReflectTools.java
 * created at : 6:08:46 PM Jun 18, 2016
 * created by 970655147
 */

package com.hx.log.util;

public class ReflectTools {

	// 判断给定的type是否实现了给定的clazz接口
	// 1. self
	// 2. implements
	// 3. recurse superTypes [1. incase of 'PrimitiveTypes', 2. incase of 'java.lang.Object' ]
	public static boolean implements0(Class<?> type, Class clazz) {
		if(type.getName().equals(clazz.getName()) ) {
			return true;
		}
		Class[] implemented = type.getInterfaces();
		for(int i=0; i<implemented.length; i++) {
			if(implemented[i].getName().equals(clazz.getName()) ) {
				return true;
			}
		}

		Class superType = type.getSuperclass();
		// incase of 'PrimitiveTypes'
		if(superType == null) {
			return false;
		}
		if("java.lang.Object".equals(superType.getName()) ) {
			return false;
		}

		return implements0(superType, clazz);
	}
	
}

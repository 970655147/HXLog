/**
 * file name : LogPattern.java
 * created at : 11:37:47 PM Apr 21, 2016
 * created by 970655147
 */

package com.hx.log.log.log_pattern.interf;

/**
 * 日志模式的接口, 各个类型
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 5:28 PM
 */
public interface LogPattern {

	/**
	 * 获取当前的pattern的结果信息
	 *
	 * @return the result of currentStartIdx LogPattern
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:20 PM
	 * @since 1.0
	 */
	String pattern();

	/**
	 * 获取当前的pattern的类型
	 *
	 * @return currentStartIdx LogPattern's LogPatternType
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:20 PM
	 * @since 1.0
	 */
	LogPatternType type();

	/**
	 * 获取当前的pattern的拷贝对象
	 *
	 * @return an copy of currentStartIdx LogPattern
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:20 PM
	 * @since 1.0
	 */
	<T extends LogPattern> T copyOf();
	
}


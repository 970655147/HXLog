/**
 * file name : LogPattern.java
 * created at : 11:37:47 PM Apr 21, 2016
 * created by 970655147
 */

package com.hx.log.interf;

// 日志模式的接口, 各个类型
public interface LogPattern {

	/**
	 * 获取当前的pattern的结果信息
	 *
	 * @return the result of current LogPattern
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:20 PM
	 * @since 1.0
	 */
	String pattern();

	/**
	 * 获取当前的pattern的类型
	 *
	 * @return current LogPattern's LogPatternType
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:20 PM
	 * @since 1.0
	 */
	LogPatternType type();

	/**
	 * 获取当前的pattern的拷贝对象
	 *
	 * @return an copy of current LogPattern
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:20 PM
	 * @since 1.0
	 */
	<T extends LogPattern> T copyOf();


	/**
	 * 一个参数变量的LogPattern
	 *
	 * @author Jerry.X.He <970655147@qq.com>
	 * @version 1.0
	 * @date 4/12/2017 10:21 PM
	 */
	abstract class OneStringVariableLogPattern implements LogPattern {
		protected String arg;

		public OneStringVariableLogPattern(String arg) {
			setArg(arg);
		}

		/**
		 * 配置参数, 以及默认的pattern实现
		 *
		 * @return
		 * @author Jerry.X.He
		 * @date 4/12/2017 10:20 PM
		 * @since 1.0
		 */
		public void setArg(String arg) {
			this.arg = arg;
		}

		@Override
		public String pattern() {
			return arg;
		}
		public LogPattern copyOf() {
			return this;
		}
	}
	
}


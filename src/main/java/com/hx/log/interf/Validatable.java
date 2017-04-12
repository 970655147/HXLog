/**
 * file name : Validateable.java
 * created at : 下午5:03:10 2016年8月25日
 * created by 970655147
 */

package com.hx.log.interf;

// Validateable, 感觉 和我以前的那个Validator好像啊['HXBlog' in 'BaseAction']
public interface Validatable {


	/**
	 * 校验给定的参数, 判断是否合法
	 *
	 * @param args 参数列表
	 * @return true if args is valid, or esle
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:19 PM
	 * @since 1.0
	 */
	boolean validate(Object... args);

	/**
	 * 获取校验参数之后, 存在的异常对象
	 *
	 * @return object represent Exception Object after last check
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:19 PM
	 * @since 1.0
	 */
	Object getLastErr();
	
}

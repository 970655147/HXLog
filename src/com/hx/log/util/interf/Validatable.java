/**
 * file name : Validateable.java
 * created at : 下午5:03:10 2016年8月25日
 * created by 970655147
 */

package com.hx.log.util.interf;

// Validateable, 感觉 和我以前的那个Validator好像啊['HXBlog' in 'BaseAction']
public interface Validatable {

	// 校验当前对象 以及 给定的参数
	// 获取错误封装对象
	public boolean validate(Object... args);
	public Object getLastErr();
	
}

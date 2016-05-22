/**
 * file name : Test03TestTools.java
 * created at : 12:59:46 PM Mar 22, 2016
 * created by 970655147
 */

package com.hx.log.test;

import net.sf.json.JSONObject;

import com.hx.log.log.Log;
import com.hx.log.log.Tools;

public class Test03TestTools {

	// 测试Tools相关方法
	public static void main(String []args) throws Exception {
		
		String str = " lkdjfg	 	 lkdfg	lksdf 	lskdfsldf";
		Log.log(Tools.trimAllSpaces(str) );
		
		Log.log(Tools.DEFAULT_CHARSET);
		
		// 以"gbk"向System.out写出数据才能够得到正确的结果, 原因在于System.out默认的解码规则是使用'项目的编码'么
		Log.log("中国");
		
		Tools.save(str, Tools.getTmpPath("abc", Tools.TXT), Tools.GBK, true );
		
		Tools.assert0(new Exception("abc") );
		
		Log.log(Tools.encapQueryString(new JSONObject().element("key1", "val1").element("key2", "val2")) );
		
		Tools.awaitShutdown();
		
	}
	
}

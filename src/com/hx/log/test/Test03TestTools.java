/**
 * file name : Test03TestTools.java
 * created at : 12:59:46 PM Mar 22, 2016
 * created by 970655147
 */

package com.hx.log.test;

import com.hx.log.util.Log;
import com.hx.log.util.Tools;
import com.hx.log.alogrithm.tree.TreeUtils;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

public class Test03TestTools {

	// 测试Tools相关方法
	public static void main(String []args) throws Exception {
		
		String str = " lkdjfg	 	 lkdfg	lksdf 	lskdfsldf";
		Log.log(Tools.trimAllSpaces(str) );
		
		Log.log(Tools.DEFAULT_CHARSET);
		
		// 以"gbk"向System.out写出数据才能够得到正确的结果, 原因在于System.out默认的解码规则是使用'项目的编码'么
		Log.log("中国");
		
		Tools.save(str, Tools.getTmpPath("abc", Tools.TXT), Tools.GBK, true );
		
//		Tools.assert0(new Exception("abc") );
		
		Log.log(Tools.encapQueryString(new JSONObject().element("key1", "val1").element("key2", "val2")) );
		
//		Tools.asMap(new String[]{"a"}, "a", "b");
		
		Log.log(Tools.camel2UnderLine("abcDef") );
		Log.log(Tools.underLine2Camel("abc_Def") );
		Log.log(Tools.underLine2Camel("abc_def") );
		
		Log.log(Tools.formatedNowStr() );
		
		Tools.IS_DEBUG_ON = false;
		Tools.assert1("abc");
		
//		TreePattern.TREE_SEPS = "+";
		TreeUtils.TREE_VERTICAL_LINE = "+";
		JSONArray treedArr = JSONArray.fromObject("[{\"type\":\"arr\",\"name\":\"directoryStructure\"},{\"type\":\"obj\",\"name\":\"file03.txt\",\"size\":3},[{\"type\":\"arr\",\"name\":\"dir01\"},{\"type\":\"obj\",\"name\":\"file01.txt\",\"size\":16}],[{\"type\":\"arr\",\"name\":\"dir02\"},{\"type\":\"obj\",\"name\":\"file02.txt\",\"size\":17}]]");
		Tools.save(TreeUtils.tree(treedArr), Tools.getTmpPath(17) );
		
		Log.log(Tools.getClazzNameByFullName("com.hx.User") );
		Log.log(Tools.getClazzNameByFullName("User") );
		
		Log.log(Tools.isCommentLine("#sdf") );
		Log.log(Tools.isCommentLine("//") );
		Log.log(Tools.isCommentLine("-----") );
		
		Tools.awaitShutdown();
		
	}
	
}

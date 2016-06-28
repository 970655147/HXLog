/**
 * file name : Test03TestTools.java
 * created at : 12:59:46 PM Mar 22, 2016
 * created by 970655147
 */

package com.hx.log.test;

import com.hx.log.log.Log;
import com.hx.log.util.Tools;

import net.sf.json.JSONObject;

public class Test03TestTools {

	// ����Tools��ط���
	public static void main(String []args) throws Exception {
		
		String str = " lkdjfg	 	 lkdfg	lksdf 	lskdfsldf";
		Log.log(Tools.trimAllSpaces(str) );
		
		Log.log(Tools.DEFAULT_CHARSET);
		
		// ��"gbk"��System.outд�����ݲ��ܹ��õ���ȷ�Ľ��, ԭ������System.outĬ�ϵĽ��������ʹ��'��Ŀ�ı���'ô
		Log.log("�й�");
		
		Tools.save(str, Tools.getTmpPath("abc", Tools.TXT), Tools.GBK, true );
		
//		Tools.assert0(new Exception("abc") );
		
		Log.log(Tools.encapQueryString(new JSONObject().element("key1", "val1").element("key2", "val2")) );
		
//		Tools.asMap(new String[]{"a"}, "a", "b");
		
		Log.log(Tools.camel2UnderLine("abcDef") );
		Log.log(Tools.underLine2Camel("abc_Def") );
		Log.log(Tools.underLine2Camel("abc_def") );
		
		Log.log(Tools.formatedNowStr() );
		
		Tools.assert1("abc");
		
		Tools.awaitShutdown();
		
	}
	
}

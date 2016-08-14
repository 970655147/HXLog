/**
 * file name : ProjectToJar.java
 * created at : 下午7:07:08 2016年8月11日
 * created by 970655147
 */

package com.hx.log.util;

import static com.hx.log.util.Log.err;
import static com.hx.log.util.Log.log;
import static java.lang.System.err;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.jar.JarInputStream;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;

public class ProjectToJar {

	// 将给定的projectPath的数据拷贝到给定的jarPath中, manifestEntrys为需要写出的清单文件的内容
	public static void updateJarAndSrc(String projectPath, String jarPath, Map<String, String> manifestEntrys, FileNameFilter filter) throws IOException {
		JarOutputStream jarJos = null;
		try {
			jarJos = new JarOutputStream(new BufferedOutputStream(new FileOutputStream(jarPath)) );
			updateJarAndSrc0(new File(projectPath), jarJos, "", 0, filter);
			writeManifest(jarJos, manifestEntrys);
		} finally {
			if(jarJos != null) {
				jarJos.close();
			}
		}
	}
	
	// 将给定的projectPath的数据拷贝到给定的jarPath中
		// 使用JarOutputStream 配合ZipEntry
	private static void updateJarAndSrc0(File projectPath, JarOutputStream jos, String prefix, int depth, FileNameFilter filter) throws IOException {
		if(projectPath.isDirectory() ) {
			filter.setPatternIdx(depth);
			File[] childs = projectPath.listFiles(filter);
			for(File child : childs) {
				if(child.isDirectory() ) {
					String childFileName = prefix + "/" + child.getName();
					if(depth == 0) {
						childFileName = child.getName();
					}
					updateJarAndSrc0(child, jos, childFileName, depth+1, filter);
				} else {
					boolean isOk = true;
					ZipEntry entry = null;
					try {
						if(depth == 0) {
							entry = new ZipEntry(child.getName());
						} else {
							entry = new ZipEntry(prefix + "/" + child.getName());
						}
						jos.putNextEntry(entry );
					} catch(Exception e) {
						isOk = false;
						err(e.getMessage() );
					}
					
					if(isOk) {
						BufferedInputStream bis = new BufferedInputStream(new FileInputStream(child) );
						byte[] buff = new byte[2048];
						int len = -1;
						
						while((len = bis.read(buff)) > 0) {
							jos.write(buff, 0, len);
						}
						jos.closeEntry();
						log("update the file : '" + child.getAbsolutePath() + "' -> '" + entry.getName() + "' success !");
					} else {
						err("update the file : '" + child.getAbsolutePath() + "' failed !");
					}
				}
			}
		}
	}
	
	// 清单文件的常量, 以及一些常量
	public static final String MANIFEST_VERSION = "Manifest-Version";
	public static final String MAIN_CLASS = "Main-Class";
	public static final String CREATED_BY = "Created-By";
	public static String DEFAULT_MANIFEST_VERSION = "1.0";
	public static String DEFAULT_MAIN_CLASS = "";
	public static String DEFAULT_CREATED_BY = "1.7.0_40 (Oracle Corporation)";
	public static String CRLF = "\r\n";
	public static String SEP = ": ";
	
	// 默认的清单文件配置manifestEntries
	private static Map<String, String> manifestEntrysTemplates = new LinkedHashMap<>();
	static {
		manifestEntrysTemplates.put(MANIFEST_VERSION, DEFAULT_MANIFEST_VERSION);
		manifestEntrysTemplates.put(MAIN_CLASS, DEFAULT_MAIN_CLASS);
		manifestEntrysTemplates.put(CREATED_BY, DEFAULT_CREATED_BY);
	}
	public static Map<String, String> newManifestEntrysTemplate() {
		return new LinkedHashMap<String, String>(manifestEntrysTemplates);
	}
	
	// 写出清单文件到给定的jar包
	private static void writeManifest(JarOutputStream jarJos, Map<String, String> manifestEntrys) throws IOException {
		jarJos.putNextEntry(new ZipEntry("META-INF/MANIFEST.MF") );
		StringBuilder sb = new StringBuilder();
		Iterator<Entry<String, String> > it = manifestEntrys.entrySet().iterator();
		while(it.hasNext() ) {
			Entry<String, String> entry = it.next();
			if((! FileNameMatcher.isEmpty(entry.getKey())) && (! FileNameMatcher.isEmpty(entry.getValue())) ) {
				sb.append(entry.getKey() );		sb.append(SEP);
				sb.append(entry.getValue() );	sb.append(CRLF);
			}
		}
		
//		Log.log(sb.toString() );
		jarJos.write(sb.toString().getBytes() );
		log("write manifest with mainClass : '" + String.valueOf(manifestEntrys.get("Main-Class")) + "'" );
	}
	
	// 测试将给定的projectPath中的数据写出到给定的jarPath对应的文件[使用Jar 'Input / Output' Stream]
	private static void testForStream(String projectPath, String jarPath) throws IOException {
//		JarFile jar = new JarFile(new File(jarPath) );
		JarInputStream jis = new JarInputStream(new BufferedInputStream(new FileInputStream(jarPath)) );
		JarOutputStream jos = new JarOutputStream(new BufferedOutputStream(new FileOutputStream("D:/1.jar")) );
		
		byte[] buff = new byte[1024];
		int len = -1;
		
		// ZipExceptione : casuse expect 1126, but got 1130 !
		// copy by JarInputStream, failed !
		ZipEntry entry = null;
		while((entry = jis.getNextEntry()) != null ) {
			log(entry.getName() + " -> " + entry.getSize() + " -> " + entry.getCompressedSize() );
//			jos.putNextEntry(entry);
//			BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(Tools.getNextTmpPath(Tools.TXT)) );
			while((len = jis.read(buff)) > 0) {
//				jos.write(buff, 0, len);
//				bos.write(buff, 0, len);
			}
//			jos.closeEntry();
//			bos.close();
		}
		jis.close();
		
		
		// copy by JarFile, failed !
//		Enumeration<JarEntry> it = jar.entries();
//		while(it.hasMoreElements() ) {
//			JarEntry entry = it.nextElement();
//			Log.log(entry.getName() );
//			jos.putNextEntry(entry);
//			BufferedInputStream bis = new BufferedInputStream(jar.getInputStream(entry) );
//			while((len = bis.read(buff)) > 0) {
//				jos.write(buff, 0, len);
//			}
//			jos.closeEntry();
//		}
		
	
//		jos.putNextEntry(new ZipEntry("a.txt"));
//		jos.write("HelloWorld.java".getBytes() );
		
		File binFolder = new File(projectPath);
		
//		jar.close();
		jos.close();
		jis.close();
		
//		new ZipEntry(name)
	}
	
	// 文件名过滤器
	public static class FileNameFilter implements FilenameFilter {
		// 给定的前缀, 以及当前需要匹配的前缀的索引
		private String[] patterns;
		private int patternIdx;
		
		// 初始化
		public FileNameFilter(String []patterns) {
			this.patterns = patterns;
		}
		
		// 业务方法, 委托给FileNameMatcher[匹配通配符等等]
		public boolean accept(File dir, String name) {
			if(patternIdx >= patterns.length) {
				return true;
			}
			
			return FileNameMatcher.match(name, patterns[patternIdx] );
		}
		
		// setter & getter
		public void setPatternIdx(int idx) {
			this.patternIdx = idx;
		}
	}
	
}
